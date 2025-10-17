from datasets import Dataset, Features, Value, Sequence
from transformers import AutoTokenizer, AutoModelForTokenClassification, TrainingArguments, Trainer
from torch import tensor
import evaluate
import numpy as np
import json

############## Load and preprocess data ##############
# 1. Load JSONL data
def load_jsonl(path):
    with open(path, "r", encoding="utf-8") as f:
        return [json.loads(line) for line in f]

train_data = load_jsonl("output/doccano/abstracts_with_cohort_info.jsonl")

def convert_label_spans_to_int(example):
    """
    Convert start/end of each label span to int if they're strings.
    """
    if "label" in example:
        new_label = []
        for label in example["label"]:
            start, end, tag = label
            new_label.append([int(start), int(end), tag])
        example["label"] = new_label
    return example

train_data = [convert_label_spans_to_int(x) for x in train_data]

def clean_labels(example):
    new_label = []
    for label in example.get("label", []):
        if len(label) == 3:
            start, end, tag = label
            new_label.append([int(start), int(end), str(tag)])
    example["label"] = new_label
    return example

train_data = [clean_labels(x) for x in train_data]

############## Split train/validation ##############

from sklearn.model_selection import train_test_split
import random

# Shuffle data first
random.seed(42)
random.shuffle(train_data)

# Split directly
train_data, val_data = train_test_split(train_data, 
                                        test_size=0.1, 
                                        random_state=42)

############## Convert to HuggingFace Dataset ##############

features = Features({
    "text": Value("string"),
    "pubmed_id": Value("int64"),
    "date": Value("string"),
    "gwas_cat_cohort_label": Value("string"),
    "label": Sequence(Sequence(Value("string")))  # optional, if you want to keep
})

train_dataset = Dataset.from_list(train_data, features=features)
val_dataset = Dataset.from_list(val_data, features=features)

#######################################################################
########### Checking labels ##########

label_list = ["O", "B-COHORT", "I-COHORT"]
id2label = {i: label for i, label in enumerate(label_list)}
label2id = {label: i for i, label in enumerate(label_list)}

# Map label IDs back to strings
id2label = {0: "O", 1: "B-COHORT", 2: "I-COHORT"}

# Check number of COHORT spans in training and validation sets
# Check that there are COHORT spans in both datasets
def count_cohort_spans(dataset):
    counts = {"COHORT": 0}
    for example in dataset:
        for span in example.get("label", []):
            if len(span) == 3:
                counts["COHORT"] += 1
    return counts

train_counts = count_cohort_spans(train_dataset)
val_counts = count_cohort_spans(val_dataset)

print("\n")
print("Training COHORT spans:", train_counts)
print("Validation COHORT spans:", val_counts)
print("\n")

############## Tokenization and alignment ##############
# 3. Load tokenizer and model
model_name = "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext"
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForTokenClassification.from_pretrained(model_name) #,  num_label=3)

model.config.id2label = id2label
model.config.label2id = label2id

# 5. Tokenize and align label
def tokenize_and_align_label(batch):
    tokenized_inputs = tokenizer(
        batch["text"],
        truncation=True,
        padding="max_length",
        max_length=256,
        return_offsets_mapping=True
    )

    all_label = []

    for i, spans in enumerate(batch["label"]):

        aligned_labels = []

        offsets = tokenized_inputs["offset_mapping"][i]
        for (token_start, token_end) in offsets:
            # Special tokens may have (0,0) or similar
            if token_start == token_end:
                aligned_labels.append(-100)
                continue

            token_label = "O"
            for span_start, span_end, span_tag in spans:
                span_start = int(span_start)
                span_end = int(span_end)
                # overlap check
                if token_end > span_start and token_start < span_end:
                    token_label = "B-COHORT" if token_start == span_start else "I-COHORT"
                    break
            aligned_labels.append(label2id[token_label])

        all_label.append(aligned_labels)    

    tokenized_inputs["label"] = all_label
    tokenized_inputs.pop("offset_mapping")
    return tokenized_inputs

    tokenized_inputs["label"] = all_label
    tokenized_inputs.pop("offset_mapping")
    return tokenized_inputs

tokenized_train = train_dataset.map(tokenize_and_align_label, batched=True)
tokenized_val = val_dataset.map(tokenize_and_align_label, batched=True)

# After tokenizing the dataset
tokenized_train = train_dataset.map(tokenize_and_align_label, batched=True)
tokenized_val = val_dataset.map(tokenize_and_align_label, batched=True)

# Check token-level label counts
def count_token_labels(tokenized_dataset, id2label):
    counts = {"B-COHORT": 0, "I-COHORT": 0, "O": 0}
    for example in tokenized_dataset:
        for l in example["label"]:
            if l != -100:
                counts[id2label[l]] += 1
    return counts

train_token_counts = count_token_labels(tokenized_train, id2label)
val_token_counts = count_token_labels(tokenized_val, id2label)

# Chech token-level label counts - match initial dataset
print("\n")
print("Token-level label counts (training):", train_token_counts)
print("Token-level label counts (validation):", val_token_counts)
print("\n")

# 6. Metrics
metric = evaluate.load("seqeval")

def compute_metrics(p):
    predictions, label = p
    predictions = np.argmax(predictions, axis=2)
    true_predictions = [
        [label_list[p] for (p, l) in zip(pred, lab) if l != -100]
        for pred, lab in zip(predictions, label)
    ]
    true_label = [
        [label_list[l] for (p, l) in zip(pred, lab) if l != -100]
        for pred, lab in zip(predictions, label)
    ]
    results = metric.compute(predictions=true_predictions, references=true_label)
    return {
        "precision": results["overall_precision"],
        "recall": results["overall_recall"],
        "f1": results["overall_f1"],
        "accuracy": results["overall_accuracy"],
    }

model_name = "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext"
num_labels = 3  # O, B-COHORT, I-COHORT

# 7. Training arguments
args = TrainingArguments(
    "pubmedbert-cohort-ner",
    learning_rate=2e-5,
    per_device_train_batch_size=16,
    num_train_epochs=5,
    weight_decay=0.05,
)

# 8. Trainer
trainer = Trainer(
    model=model,
    args=args,
    train_dataset=tokenized_train,
    eval_dataset=tokenized_val,
    tokenizer=tokenizer,
    compute_metrics=compute_metrics
)

# 9. Train
trainer.train()

# 10. Save model
trainer.save_model("pubmedbert-cohort-ner-model")
tokenizer.save_pretrained("pubmedbert-cohort-ner-model")


# Evaluate the trained model on the validation set
eval_results = trainer.evaluate(tokenized_val)

print("=== Overall Metrics ===")
print(f"Validation Loss: {eval_results['eval_loss']:.4f}")
print(f"Overall Precision: {eval_results['eval_precision']:.4f}")
print(f"Overall Recall:    {eval_results['eval_recall']:.4f}")
print(f"Overall F1:        {eval_results['eval_f1']:.4f}")
print(f"Overall Accuracy:  {eval_results['eval_accuracy']:.4f}")

# Detailed per-label metrics
if "eval_overall_precision" not in eval_results:  # sometimes keys vary
    eval_results_keys = eval_results.keys()
else:
    eval_results_keys = eval_results.keys()

# Some HuggingFace versions return the seqeval dict as `eval_seqeval`
if "eval_seqeval" in eval_results:
    seqeval_results = eval_results["eval_seqeval"]
else:
    seqeval_results = eval_results

print("\n=== Per-Label Metrics ===")
for label in label_list:
    if label in seqeval_results:
        print(f"{label}: {seqeval_results[label]}")


predictions, labels, _ = trainer.predict(tokenized_val)
pred_labels = np.argmax(predictions, axis=2)

num_pred_entities = np.sum(pred_labels != 0)
print("Number of predicted entity tokens:", num_pred_entities)

# Optional: check percentage of predicted entities
total_tokens = np.prod(pred_labels.shape)
print("Percentage of predicted entity tokens:", num_pred_entities / total_tokens * 100, "%")