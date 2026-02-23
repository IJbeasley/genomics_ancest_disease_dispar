from datasets import Dataset, Features, Value, Sequence
from transformers import AutoTokenizer, AutoModelForTokenClassification, AutoConfig, TrainingArguments, Trainer, DataCollatorForTokenClassification
from torch import tensor
import evaluate
import numpy as np
import json
import argparse
import os
import random
import argparse
import os

# Parse command-line arguments
parser = argparse.ArgumentParser(description="Train or evaluate PubMedBERT for cohort NER")
parser.add_argument(
    "--skip-training",
    action="store_true",
    help="Skip training and use previously fine-tuned model from pubmedbert-cohort-ner-model/"
)
parser.add_argument(
    "--seed",
    type=int,
    default=102,
    help="Random seed for reproducibility (default: 102)"
)
parser.add_argument(
    "--model-path",
    type=str,
    default="pubmedbert-cohort-ner-model",
    help="Path to save/load the fine-tuned model (default: pubmedbert-cohort-ner-model)"
)
args_parsed = parser.parse_args()

############## Load and preprocess data ##############
# 1. Load JSONL data
def load_jsonl(path):
    with open(path, "r", encoding="utf-8") as f:
        return [json.loads(line) for line in f]

train_data = load_jsonl("output/doccano/abstracts_with_cohort_info.jsonl")
random.seed(args_parsed.seed)
train_data = random.sample(train_data, k=1000)  # Sample 1000 random lines

def normalize_record(example):
    """
    Normalize record types for HF Datasets.
    - pubmed_id: int64 (fallback -1)
    - date/country/gwas_cat_cohort_label: string (empty if dict/None)
    - label: list of [start, end, tag] as strings
    """
    pubmed_id = example.get("pubmed_id", -1)
    try:
        example["pubmed_id"] = int(pubmed_id)
    except (TypeError, ValueError):
        example["pubmed_id"] = -1

    for key in ["date", "country", "gwas_cat_cohort_label"]:
        value = example.get(key, "")
        if isinstance(value, dict) or value is None:
            example[key] = ""
        else:
            example[key] = str(value)

    new_label = []
    for label in example.get("label", []):
        if isinstance(label, (list, tuple)) and len(label) >= 3:
            start, end, tag = label[0], label[1], label[2]
            new_label.append([str(start), str(end), str(tag)])
    example["label"] = new_label
    return example

train_data = [normalize_record(x) for x in train_data]

############## Split train/validation ##############

from sklearn.model_selection import StratifiedGroupKFold
import numpy as np

# Create binary stratification target: whether sample has any COHORT labels
stratify_target = np.array([1 if any(label[2] == "COHORT" for label in example.get("label", [])) else 0 
                            for example in train_data])
groups = np.array([example.get("pubmed_id", -1) for example in train_data])

# Use StratifiedGroupKFold (n_splits=10, take first fold)
sgkf = StratifiedGroupKFold(n_splits=10, shuffle=True, random_state=args_parsed.seed)
split_iter = sgkf.split(train_data, y=stratify_target, groups=groups)
train_idx, val_idx = next(split_iter)

# Split data using indices
train_data = [train_data[i] for i in train_idx]
val_data = [train_data[i] for i in val_idx]

############## Convert to HuggingFace Dataset ##############

features = Features({
    "text": Value("string"),
    "pubmed_id": Value("int64"),
    "date": Value("string"),
    "country": Value("string"),
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
config = AutoConfig.from_pretrained(
    model_name,
    num_labels=3,
    hidden_dropout_prob=0.1,
    attention_probs_dropout_prob=0.1,
)
model = AutoModelForTokenClassification.from_pretrained(model_name, 
                                                        config=config)

model.config.id2label = id2label
model.config.label2id = label2id



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
    results = metric.compute(predictions=true_predictions, 
                             references=true_label)
    return {
        "precision": results["overall_precision"],
        "recall": results["overall_recall"],
        "f1": results["overall_f1"],
        "accuracy": results["overall_accuracy"],
    }







# 9. Train (or skip if using pre-trained model)
if args_parsed.skip_training:
    if os.path.exists(args_parsed.model_path):
        print(f"\n=== Loading pre-trained model from {args_parsed.model_path} ===")
        model = AutoModelForTokenClassification.from_pretrained(args_parsed.model_path)
        tokenizer = AutoTokenizer.from_pretrained(args_parsed.model_path)
        model.config.id2label = id2label
        model.config.label2id = label2id
        print(f"Loaded model and tokenizer from {args_parsed.model_path}/\n")
    else:
        raise FileNotFoundError(f"Model directory {args_parsed.model_path}/ not found. Train first without --skip-training")
else:
    trainer.train()

    model_name = "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext"
    
    num_labels = 3  # O, B-COHORT, I-COHORT
    # 8. Trainer
    data_collator = DataCollatorForTokenClassification(tokenizer)
    
    trainer = Trainer(model=model,
                      args=args,
                      train_dataset=tokenized_train,
                      eval_dataset=tokenized_val,
                      data_collator=data_collator,
                      compute_metrics=compute_metrics
                      )
    
    # 7. Training arguments
    args = TrainingArguments("pubmedbert-cohort-ner",
                             learning_rate=1e-5, #rates to try: 1e-05, 3e-5, 5e-5
                             per_device_train_batch_size=16, # rate to try: 16, 32
                             num_train_epochs=5 # rates to try: 3, 5, 10
                             #weight_decay=0.1, # rates to try: 0.01, 0.05, 0.1
                             )
    
    # 10. Save model
    trainer.save_model(args_parsed.model_path)
    tokenizer.save_pretrained(args_parsed.model_path)
    print(f"\n=== Model and tokenizer saved to {args_parsed.model_path} ===")


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

# === Save predictions ===
id2label = {i: label for i, label in enumerate(label_list)}

# Convert token IDs back to tokens and labels
tokenized_texts = tokenized_val["input_ids"]
tokens = [tokenizer.convert_ids_to_tokens(seq) for seq in tokenized_texts]
true_labels = [[id2label[label_id] if label_id != -100 else "PAD" for label_id in seq] for seq in labels]
pred_labels_named = [[id2label[label_id] for label_id in seq] for seq in pred_labels]

# Flatten and structure as dataframe
all_records = []
for i in range(len(tokens)):
    for token, true_label, pred_label in zip(tokens[i], true_labels[i], pred_labels_named[i]):
        if token not in ["[PAD]", "[CLS]", "[SEP]"]:  # ignore special tokens
            all_records.append({
                "abstract_id": i,
                "token": token,
                "true_label": true_label,
                "pred_label": pred_label
            })

import pandas as pd

df_preds = pd.DataFrame(all_records)

# Save to CSV and JSONL
df_preds.to_csv("output/pubmedbert_predictions.csv", index=False)
df_preds.to_json("output/pubmedbert_predictions.jsonl", orient="records", lines=True)

print(f"Saved {len(df_preds)} token-level predictions to output/pubmedbert_predictions.csv and .jsonl")

# === Extract entity spans and save as text ===
def extract_entities_from_tokens(tokens, labels, label_list, id2label):
    """Extract entity spans from token-level labels."""
    entities = []
    current_entity = None
    current_label = None
    
    for token, label_id in zip(tokens, labels):
        # Skip special tokens and label -100
        if label_id == -100 or token in ["[PAD]", "[CLS]", "[SEP]"]:
            continue
        
        label = id2label[label_id]
        
        if label == "O":
            if current_entity:
                entities.append((current_entity, current_label))
                current_entity = None
                current_label = None
        elif label.startswith("B-"):
            if current_entity:
                entities.append((current_entity, current_label))
            current_label = label[2:]  # Remove "B-" prefix
            current_entity = token.replace("##", "")
        elif label.startswith("I-"):
            if current_entity:
                current_entity += token.replace("##", "")
            else:
                current_entity = token.replace("##", "")
    
    if current_entity:
        entities.append((current_entity, current_label))
    
    return entities

# Reconstruct text from tokens and extract entities
import json

def merge_consecutive_entities(entities):
    """Merge consecutive entities with the same label."""
    if not entities:
        return entities
    
    merged = []
    current_text = entities[0][0]
    current_label = entities[0][1]
    
    for i in range(1, len(entities)):
        text, label = entities[i]
        if label == current_label:
            # Same label: merge with space
            current_text += " " + text
        else:
            # Different label: save current and start new
            merged.append((current_text, current_label))
            current_text = text
            current_label = label
    
    # Add the last entity
    merged.append((current_text, current_label))
    return merged

entity_records = []
for i in range(len(tokens)):
    # Reconstruct text from tokens
    text_tokens = tokens[i]
    reconstructed_text = tokenizer.decode(
        tokenizer.convert_tokens_to_ids(text_tokens),
        skip_special_tokens=True
    )
    
    # Extract true and predicted entities
    true_entities = extract_entities_from_tokens(
        text_tokens, labels[i], label_list, id2label
    )
    true_entities = merge_consecutive_entities(true_entities)
    
    pred_entities = extract_entities_from_tokens(
        text_tokens, pred_labels[i], label_list, id2label
    )
    pred_entities = merge_consecutive_entities(pred_entities)
    
    # Get original pubmed_id from validation dataset
    pubmed_id = val_data[i].get("pubmed_id", -1)
    
    # Format entity text
    true_cohorts = ", ".join([entity[0] for entity in true_entities if entity[1] == "COHORT"])
    pred_cohorts = ", ".join([entity[0] for entity in pred_entities if entity[1] == "COHORT"])
    
    entity_records.append({
        "pubmed_id": pubmed_id,
        "sentence": reconstructed_text,
        "true_cohorts": true_cohorts,
        "predicted_cohorts": pred_cohorts
    })

# Save entity-level predictions
with open("output/pubmedbert_entity_predictions.jsonl", "w") as f:
    for record in entity_records:
        f.write(json.dumps(record) + "\n")

df_entity = pd.DataFrame(entity_records)
df_entity.to_csv("output/pubmedbert_entity_predictions.csv", index=False)

print(f"Saved {len(entity_records)} entity-level predictions to output/pubmedbert_entity_predictions.csv and .jsonl")