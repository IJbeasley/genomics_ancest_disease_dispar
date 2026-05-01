from datasets import Dataset, Features, Value, Sequence
from transformers import AutoTokenizer, AutoModelForTokenClassification, AutoConfig, TrainingArguments, Trainer, DataCollatorForTokenClassification
from torch import tensor
import warnings
import transformers 
import torch
import evaluate
import numpy as np
import pandas as pd
import json
import argparse
import os
import random
import sys

# Make sibling modules in this directory importable regardless of cwd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tokenise_data import get_tokenized_datasets, tokenize_dataset

# Parse command-line arguments
parser = argparse.ArgumentParser(description="Train or evaluate PubMedBERT for cohort NER")
parser.add_argument(
    "--skip_training",
    action="store_true",
    help="Skip training and use previously fine-tuned model from pubmedbert-cohort-ner-model/"
)
parser.add_argument(
    "--entity_types",
    type=str,
    default="COHORT,REF_PANEL,FUNC_DATA",
    help="Comma-separated list of entity types for NER (default: COHORT,REF_PANEL)"
)
parser.add_argument(
    "--seed",
    type=int,
    default=102,
    help="Random seed for reproducibility (default: 102)"
)
parser.add_argument(
    "--model_path",
    type=str,
    default="pubmedbert-cohort-ner-model",
    help="Path to save/load the fine-tuned model (default: pubmedbert-cohort-ner-model)"
)
parser.add_argument(
    "--huggingface_model",
    type=str,
    default="microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext",
    help="Name of the huggingface model (default PubMedBERT). " \
    "Options include:" \
    "PubMedBERT versions:"
    "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext, " \
    "microsoft/BiomedNLP-BiomedBERT-large-uncased-abstract, " \
    "microsoft/BiomedNLP-BiomedBERT-base-uncased-abstract" \
    "bioformer versions:"
    "bioformers/bioformer-8L" \
    "bioformers/bioformer-16L"
)
#model_name = "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext"
# pubmedbert options:
# microsoft/BiomedNLP-BiomedBERT-large-uncased-abstract
# microsoft/BiomedNLP-BiomedBERT-base-uncased-abstract
# model_name = "microsoft/BiomedNLP-BiomedBERT-large-uncased-abstract"

# biobert options:
# https://huggingface.co/collections/dmis-lab/biobert
# dmis-lab/biobert-large-cased-v1.1
# dmis-lab/biobert-base-cased-v1.1
# model_name = "dmis-lab/biobert-base-cased-v1.1"
parser.add_argument(
    "--test-path",
    type=str,
    default=None,
    help=(
        "Optional path to a JSONL test file. Only used with --skip_training: "
        "the loaded model is applied to this file instead of the validation split. "
        "Records may include Doccano-style 'label' spans (will be scored) or omit "
        "them (predictions only). Requires at least a 'text' field per line."
    ),
)
parser.add_argument(
    "--train_path",
    type=str,
    default="output/doccano/abstracts_with_cohort_info_reconstructed.jsonl",
    help=(
        "Path to a jsonl file of training data"
    ),
)
parser.add_argument(
    "--output_path",
    type=str,
    default="output/text_mining_predictions",
    help=(
        "Path to save model predictions"
    ),
)

args_parsed = parser.parse_args()
transformers.set_seed(args_parsed.seed)
torch.manual_seed(args_parsed.seed)

############## Load and preprocess data ##############
# 1. Load and normalise JSONL data
def load_jsonl(path):
    with open(path, "r", encoding="utf-8") as f:
        return [json.loads(line) for line in f]
      
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
  
# Hugging face features
features = Features({
    "text": Value("string"),
    "pubmed_id": Value("int64"),
    "date": Value("string"),
    "country": Value("string"),
    "gwas_cat_cohort_label": Value("string"),
    "label": Sequence(Sequence(Value("string")))  # optional, if you want to keep
})

if not args_parsed.skip_training:
  
       random.seed(args_parsed.seed)
       train_data = load_jsonl(args_parsed.train_path)
       train_data = [normalize_record(x) for x in train_data]

       declared_entity_types = [t.strip() for t in args_parsed.entity_types.split(",") if t.strip()]
       seen_tags = {span[2] for ex in train_data for span in ex.get("label", [])}
       
       missing_declared = [
           entity_type for entity_type in declared_entity_types
           if entity_type not in seen_tags
           ]
       
       unexpected_tags = sorted(seen_tags - set(declared_entity_types))

       if unexpected_tags:
           warnings.warn(
        "Ignoring entity types found in training data but not declared in "
        f"--entity_types: {unexpected_tags}. Declared: {declared_entity_types}",
        stacklevel=2,
    )

       
       if missing_declared:
           raise ValueError(
        "Training labels do not match --entity_types. "
        f"Missing declared entity types: {missing_declared}. "
        # f"Unexpected training tags: {unexpected_tags}. "
        # f"Declared: {declared_entity_types}"
         )

    #    seen_tags = {span[2] for ex in train_data for span in ex.get("label", [])}
    #    unknown   = seen_tags - set(entity_types)
    #    if unknown:
    #        raise ValueError(
    #            f"Found tags in training data not declared in --entity_types: {sorted(unknown)}. "
    #            f"Declared: {entity_types}"
    #            )
       

############## Split train/validation ##############

       from sklearn.model_selection import StratifiedGroupKFold
       import numpy as np

       # Create binary stratification target: whether sample has any COHORT labels
       # stratify_target = np.array([1 if any(label[2] == "COHORT" for label in example.get("label", [])) else 0 
       #                     for example in train_data])
       # groups = np.array([example.get("pubmed_id", -1) for example in train_data])

       # Create binary stratification target: whether sample has any entity labels (any type)
       stratify_target = np.array([1 if len(example.get("label", [])) > 0 else 0 
                            for example in train_data])
       groups = np.array([example.get("pubmed_id", -1) for example in train_data])

       from collections import Counter
       if Counter(groups)[-1] > 0:
          raise ValueError(f"Warning: {Counter(groups)[-1]} records have pubmed_id = -1, which will all be in the same fold. Consider removing or fixing these records for better stratification.")

       # Use StratifiedGroupKFold (n_splits=5, take first fold)
       sgkf = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=args_parsed.seed)
       split_iter = sgkf.split(train_data, y=stratify_target, groups=groups)
       train_idx, val_idx = next(split_iter)

       # Split data using indices
       val_data   = [train_data[i] for i in val_idx]
       train_data = [train_data[i] for i in train_idx]

############## Convert to HuggingFace Dataset ##############

       train_dataset = Dataset.from_list(train_data, features=features)
       val_dataset = Dataset.from_list(val_data, features=features)

#######################################################################
########### Checking labels ##########
# Entities: comma-separated string allowed via args (optional); default keeps COHORT
entity_types = [t.strip() for t in args_parsed.entity_types.split(",") if t.strip()]
# Build BIO label list and mappings from entity_types
label_list = ["O"] + [f"B-{t}" for t in entity_types] + [f"I-{t}" for t in entity_types]
label2id = {label: i for i, label in enumerate(label_list)}
id2label = {i: label for i, label in enumerate(label_list)}
num_labels = len(label_list)
# entity_types = getattr(args_parsed, "entity_types", None)
# if entity_types is None:
#     entity_types = ["COHORT"]
# elif isinstance(entity_types, str):
#     entity_types = [t.strip() for t in entity_types.split(",") if t.strip()]

# Build BIO label list and mappings from entity_types
# label_list = ["O"] + [f"B-{t}" for t in entity_types] + [f"I-{t}" for t in entity_types]
# label2id = {label: i for i, label in enumerate(label_list)}
# id2label = {i: label for i, label in enumerate(label_list)}
# num_labels = len(label_list)

# Count spans per entity (entity-agnostic)
def count_entity_spans(dataset):
    counts = {t: 0 for t in entity_types}
    for example in dataset:
        for span in example.get("label", []):
            if len(span) >= 3:
                tag = span[2]
                counts[tag] = counts.get(tag, 0) + 1
    return counts

# if not args_parsed.skip_training:
#     train_counts = count_entity_spans(train_dataset)
#     val_counts = count_entity_spans(val_dataset)
#     print("\n")
#     print("Training entity spans:", train_counts)
#     print("Validation entity spans:", val_counts)
#     print("\n")

# label_list = ["O", "B-COHORT", "I-COHORT"]
# id2label = {i: label for i, label in enumerate(label_list)}
# label2id = {label: i for i, label in enumerate(label_list)}

# # Map label IDs back to strings
# id2label = {0: "O", 1: "B-COHORT", 2: "I-COHORT"}

# # Check number of COHORT spans in training and validation sets
# # Check that there are COHORT spans in both datasets
# def count_cohort_spans(dataset):
#     counts = {"COHORT": 0}
#     for example in dataset:
#         for span in example.get("label", []):
#             if len(span) == 3:
#                 counts["COHORT"] += 1
#     return counts

if not args_parsed.skip_training:
  
       train_counts = count_entity_spans(train_dataset)
       val_counts = count_entity_spans(val_dataset)
       
       print("\n")
       print("Training entity spans:", train_counts)
       print("Validation entity spans:", val_counts)
       print("\n")

############## Configure tokenization and alignment ##############
# load pre-trained PubMedBERT tokenizer and model
model_name = args_parsed.huggingface_model

if not args_parsed.skip_training:
  
   tokenizer = AutoTokenizer.from_pretrained(model_name)
   
   config = AutoConfig.from_pretrained(
    model_name,
    num_labels=num_labels,
    hidden_dropout_prob=0.1,
    attention_probs_dropout_prob=0.1)
    
   model = AutoModelForTokenClassification.from_pretrained(model_name, 
                                                        config=config)
                                                        
    # 5. Tokenize and align labels (imported from tokenise_data.py)
   tokenized_train, tokenized_val = get_tokenized_datasets(
           train_dataset=train_dataset,
           val_dataset=val_dataset,
           tokenizer=tokenizer,
           label2id=label2id,
           id2label=id2label
           )                                                    

else:
   if os.path.exists(args_parsed.model_path):
        print(f"\n=== Loading pre-trained model from {args_parsed.model_path} ===")
        model = AutoModelForTokenClassification.from_pretrained(args_parsed.model_path)
        tokenizer = AutoTokenizer.from_pretrained(args_parsed.model_path)
        print(f"Loaded model and tokenizer from {args_parsed.model_path}/\n")
   else:
        raise FileNotFoundError(
            f"Model directory {args_parsed.model_path}/ not found. "
            "Train first without --skip_training"
        )      
  

model.config.id2label = id2label
model.config.label2id = label2id

# If --test-path is provided alongside --skip_training, load and tokenize a
# test JSONL and use it (instead of the validation split) for final evaluation
# and predictions. Otherwise fall back to the held-out validation split.
if args_parsed.test_path:
    print(f"\n=== Loading test data from {args_parsed.test_path} ===")
    test_data = load_jsonl(args_parsed.test_path)
    test_data = [normalize_record(x) for x in test_data]
    test_dataset = Dataset.from_list(test_data, features=features)
    tokenized_test = tokenize_dataset(test_dataset, tokenizer, label2id, id2label)
    print(f"Loaded {len(test_data)} test examples\n")

    eval_dataset_tokenized = tokenized_test
    eval_raw_data = test_data
    output_prefix = "test"
else:
    if args_parsed.skip_training:
        raise ValueError(
            "--skip_training requires --test_path because the validation split "
            "is only created during training."
        )

    eval_dataset_tokenized = tokenized_val
    eval_raw_data = val_data
    output_prefix = "validation"

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
    
    # Extract overall metrics
    return {
        "precision": results["overall_precision"],
        "recall": results["overall_recall"],
        "f1": results["overall_f1"],
        "accuracy": results["overall_accuracy"],
    }

# def compute_metrics(p):
#     predictions, label = p
#     predictions = np.argmax(predictions, axis=2)
#     true_predictions = [
#         [label_list[p] for (p, l) in zip(pred, lab) if l != -100]
#         for pred, lab in zip(predictions, label)
#     ]
#     true_label = [
#         [label_list[l] for (p, l) in zip(pred, lab) if l != -100]
#         for pred, lab in zip(predictions, label)
#     ]
#     results = metric.compute(predictions=true_predictions, 
#                              references=true_label)
#     return {
#         "precision": results["overall_precision"],
#         "recall": results["overall_recall"],
#         "f1": results["overall_f1"],
#         "accuracy": results["overall_accuracy"],
#     }


# 8. Data collator + Trainer (built in both branches)
data_collator = DataCollatorForTokenClassification(tokenizer)

training_args = TrainingArguments(
    output_dir="pubmedbert-cohort-ner",
    learning_rate=1e-5,                 # rates to try: 1e-5, 3e-5, 5e-5
    per_device_train_batch_size=16,     # rates to try: 16, 32
    per_device_eval_batch_size=32,
    num_train_epochs=5,                 # rates to try: 3, 5, 10
    # weight_decay=0.1,                 # rates to try: 0.01, 0.05, 0.1
    seed=args_parsed.seed
)

trainer_kwargs = {
    "model": model,
    "args": training_args,
    "data_collator": data_collator,
    "compute_metrics": compute_metrics,
}

if not args_parsed.skip_training:
    trainer_kwargs["train_dataset"] = tokenized_train
    trainer_kwargs["eval_dataset"] = tokenized_val

trainer = Trainer(**trainer_kwargs)

# 9. Train (or skip if using pre-trained model)
# if args_parsed.skip_training:
#     if os.path.exists(args_parsed.model_path):
#         print(f"\n=== Loading pre-trained model from {args_parsed.model_path} ===")
#         model = AutoModelForTokenClassification.from_pretrained(args_parsed.model_path)
#         tokenizer = AutoTokenizer.from_pretrained(args_parsed.model_path)
#         model.config.id2label = id2label
#         model.config.label2id = label2id
#         print(f"Loaded model and tokenizer from {args_parsed.model_path}/\n")
#     else:
#         raise FileNotFoundError(
#             f"Model directory {args_parsed.model_path}/ not found. "
#             "Train first without --skip_training"
#         )

if not args_parsed.skip_training:
# Hyperparameter tuning was performed using different values for learning rate (1e-05, 3e-5, 5e-5), 
# sequence length (128, 256, 512), 
# batch size (16, 32) 
# and dropout rate (0.1) to select the model that achieved the best loss on validation set. 
    trainer.train()

    # 10. Save model
    trainer.save_model(args_parsed.model_path)
    tokenizer.save_pretrained(args_parsed.model_path)
    print(f"\n=== Model and tokenizer saved to {args_parsed.model_path} ===")


# Evaluate the model on the held-out dataset (validation split by default,
# test set if --test-path was provided with --skip_training).
eval_results = trainer.evaluate(eval_dataset_tokenized)

print("=== Overall Metrics ===")
print(f"Validation Loss: {eval_results['eval_loss']:.4f}")
print(f"Overall Precision: {eval_results['eval_precision']:.4f}")
print(f"Overall Recall:    {eval_results['eval_recall']:.4f}")
print(f"Overall F1:        {eval_results['eval_f1']:.4f}")
print(f"Overall Accuracy:  {eval_results['eval_accuracy']:.4f}")

# Recompute full results to get per-entity breakdown
predictions, labels, _ = trainer.predict(eval_dataset_tokenized)
pred_labels = np.argmax(predictions, axis=2)
true_predictions = [
    [label_list[p] for (p, l) in zip(pred, lab) if l != -100]
    for pred, lab in zip(pred_labels, labels)
]
true_label = [
    [label_list[l] for (p, l) in zip(pred, lab) if l != -100]
    for pred, lab in zip(pred_labels, labels)
]
full_results = metric.compute(predictions=true_predictions, references=true_label)

import scipy

# Softmax probabilities, shape (examples, tokens, num_labels)
probs = scipy.special.softmax(predictions, axis=2)

# For each token, confidence = max prob over labels
# Per-example: mean confidence on entity tokens only
per_example_conf = []
for i, (prob_seq, lab_seq) in enumerate(zip(probs, labels)):
    entity_mask = (lab_seq != -100) & (lab_seq != 0)  # non-O, non-padding
    if entity_mask.any():
        conf = prob_seq[entity_mask].max(axis=1).mean()
    else:
        conf = 1.0  # no entities → trivially confident
    per_example_conf.append(conf)
# save confidences to a CSV
df_conf = pd.DataFrame({
    "text": [eval_raw_data[i]["text"] for i in range(len(per_example_conf))],
    "pred_label": [true_predictions[i] for i in range(len(per_example_conf))],
    "true_label": [true_label[i] for i in range(len(per_example_conf))],
    "confidence": per_example_conf})

conf_csv_path = f"{args_parsed.output_path}/validation_confidences.csv"

print(f"\n=== Saving per-example confidences to {conf_csv_path} ===")
df_conf.to_csv(conf_csv_path, index=False)
    
# Also get confidence for each training example

if not args_parsed.skip_training:
    train_predictions, labels, _  = trainer.predict(tokenized_train)
    train_probs = scipy.special.softmax(train_predictions, axis=2)
    train_per_example_conf = []
    for i, (prob_seq, lab_seq) in enumerate(zip(train_probs, labels)):
        entity_mask = (lab_seq != -100) & (lab_seq != 0)
        if entity_mask.any():
            conf = prob_seq[entity_mask].max(axis=1).mean()
        else:
            conf = 1.0
        train_per_example_conf.append(conf)
    # save train confidences to a CSV
    df_train_conf = pd.DataFrame({
        "text": [train_data[i]["text"] for i in range(len(train_per_example_conf))],
        "pred_label": [true_predictions[i] for i in range(len(train_per_example_conf))],
        "true_label": [true_label[i] for i in range(len(train_per_example_conf))],
        "confidence": train_per_example_conf})
    
    train_conf_csv_path = f"{args_parsed.output_path}/training_confidences.csv"
    print(f"\n=== Saving per-example training confidences to {train_conf_csv_path} ===")
    df_train_conf.to_csv(train_conf_csv_path, index=False)


print("\n=== Per-Entity Metrics ===")
for entity_type in entity_types:
    if entity_type in full_results:
        entity_metrics = full_results[entity_type]
        print(f"{entity_type}:")
        print(f"  Precision: {entity_metrics.get('precision', 0):.4f}")
        print(f"  Recall:    {entity_metrics.get('recall', 0):.4f}")
        print(f"  F1:        {entity_metrics.get('f1', 0):.4f}")


# Flatten all tokens for confusion matrix
flat_true = [t for seq in true_label for t in seq]
flat_pred = [t for seq in true_predictions for t in seq]

from sklearn.metrics import confusion_matrix
labels_cm = sorted(set(flat_true) | set(flat_pred))
cm = confusion_matrix(flat_true, flat_pred, labels=labels_cm)
print("\n=== Confusion Matrix (token-level) ===")
print(pd.DataFrame(cm, index=labels_cm, columns=labels_cm))

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


predictions, labels, _ = trainer.predict(eval_dataset_tokenized)
pred_labels = np.argmax(predictions, axis=2)

num_pred_entities = np.sum(pred_labels != 0)
print("Number of predicted entity tokens:", num_pred_entities)

# Optional: check percentage of predicted entities
total_tokens = np.prod(pred_labels.shape)
print("Percentage of predicted entity tokens:", num_pred_entities / total_tokens * 100, "%")

# === Save predictions ===
id2label = {i: label for i, label in enumerate(label_list)}

# Convert token IDs back to tokens and labels
tokenized_texts = eval_dataset_tokenized["input_ids"]
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

df_preds = pd.DataFrame(all_records)

# Save to CSV and JSONL
token_csv_path = f"{args_parsed.output_path}/{output_prefix}_predictions.csv"
token_jsonl_path = f"{args_parsed.output_path}/{output_prefix}_predictions.jsonl"
df_preds.to_csv(token_csv_path, index=False)
df_preds.to_json(token_jsonl_path, orient="records", lines=True)

print(f"Saved {len(df_preds)} token-level predictions to {token_csv_path} and {token_jsonl_path}")

# === Extract entity spans and save as text ===
def extract_entities_from_tokens(tokens, labels, 
                                 label_list,
                                 id2label):
    entities = []
    current_tokens = []
    current_label = None

    def flush():
        nonlocal current_tokens, current_label
        if current_tokens:
            text = ""
            for t in current_tokens:
                if t.startswith("##"):
                    text += t[2:]
                else:
                    text += (" " if text else "") + t
            entities.append((text, current_label))
        current_tokens = []
        current_label = None

    for token, label_id in zip(tokens, labels):
        if label_id == -100 or token in ["[PAD]", "[CLS]", "[SEP]"]:
            continue
        label = id2label[label_id]

        if label == "O":
            flush()
        elif label.startswith("B-"):
            flush()
            current_label = label[2:]
            current_tokens = [token]
        elif label.startswith("I-"):
            tag = label[2:]
            if current_label == tag and current_tokens:
                current_tokens.append(token)
            else:
                # dangling I- → treat as start of a new span
                flush()
                current_label = tag
                current_tokens = [token]

    flush()
    return entities

def extract_entity_spans(text, 
                         tokenizer, 
                         true_label_ids, 
                         pred_label_ids, 
                         id2label, 
                         max_length=512):
    """
    Re-tokenize `text` with offset_mapping and extract entity spans with character
    offsets into the original text. Uses true_label_ids' -100 positions to mark
    subtoken continuations for BOTH true and predicted streams, mirroring how
    evaluation treats word-first subtokens.

    Returns (true_entities, pred_entities); each is a list of
    (surface_text, label, start_char, end_char).
    """
    enc = tokenizer(
        text,
        return_offsets_mapping=True,
        truncation=True,
        max_length=max_length,
        padding="max_length",
    )
    offsets = enc["offset_mapping"]

    def extract(stream_label_ids):
        spans = []
        cur_start = cur_end = cur_label = None

        def close():
            nonlocal cur_start, cur_end, cur_label
            if cur_start is not None:
                spans.append((text[cur_start:cur_end], cur_label, cur_start, cur_end))
            cur_start = cur_end = cur_label = None

        for t_id, s_id, (start, end) in zip(true_label_ids, stream_label_ids, offsets):
            # Special tokens / padding have (0, 0) offsets
            if start == 0 and end == 0:
                continue
            # Subtoken continuation of a word: extend current span, ignore label
            if t_id == -100:
                if cur_start is not None:
                    cur_end = end
                continue

            label = id2label[int(s_id)]
            if label == "O":
                close()
            elif label.startswith("B-"):
                close()
                cur_label = label[2:]
                cur_start, cur_end = start, end
            elif label.startswith("I-"):
                tag = label[2:]
                if cur_start is not None and cur_label == tag:
                    cur_end = end
                else:  # dangling I- → start a new span
                    close()
                    cur_label = tag
                    cur_start, cur_end = start, end

        close()
        return spans

    return extract(true_label_ids), extract(pred_label_ids)



entity_records = []
for i in range(len(tokens)):
    original_text = eval_raw_data[i]["text"]

    true_spans, pred_spans = extract_entity_spans(
        original_text,
        tokenizer,
        labels[i],
        pred_labels[i],
        id2label,
    )

    #true_cohorts = [s for s in true_spans if s[1] == "COHORT"]
    #pred_cohorts = [s for s in pred_spans if s[1] == "COHORT"]

    record = {
    "pubmed_id": eval_raw_data[i].get("pubmed_id", -1),
    "sentence":  original_text,
    }

    for etype in entity_types:
        t_spans = [s for s in true_spans if s[1] == etype]
        p_spans = [s for s in pred_spans if s[1] == etype]
        key = etype.lower()
        record[f"true_{key}s"]              = "; ".join(s[0] for s in t_spans)
        record[f"true_{key}_starts"]        = "; ".join(str(s[2]) for s in t_spans)
        record[f"true_{key}_ends"]          = "; ".join(str(s[3]) for s in t_spans)
        record[f"predicted_{key}s"]         = "; ".join(s[0] for s in p_spans)
        record[f"predicted_{key}_starts"]   = "; ".join(str(s[2]) for s in p_spans)
        record[f"predicted_{key}_ends"]     = "; ".join(str(s[3]) for s in p_spans)
    entity_records.append(record)

    # entity_records.append({
    #     "pubmed_id": eval_raw_data[i].get("pubmed_id", -1),
    #     "sentence": original_text,  # <-- use the original text so offsets line up
    #     "true_cohorts":          "; ".join(s[0] for s in true_cohorts),
    #     "true_cohort_starts":    "; ".join(str(s[2]) for s in true_cohorts),
    #     "true_cohort_ends":      "; ".join(str(s[3]) for s in true_cohorts),
    #     "predicted_cohorts":     "; ".join(s[0] for s in pred_cohorts),
    #     "predicted_cohort_starts": "; ".join(str(s[2]) for s in pred_cohorts),
    #     "predicted_cohort_ends":   "; ".join(str(s[3]) for s in pred_cohorts),
    # })

# Save entity-level predictions
entity_jsonl_path = f"{args_parsed.output_path}/{output_prefix}_entity_predictions.jsonl"
entity_csv_path = f"{args_parsed.output_path}/{output_prefix}_entity_predictions.csv"

with open(entity_jsonl_path, "w") as f:
    for record in entity_records:
        f.write(json.dumps(record) + "\n")

df_entity = pd.DataFrame(entity_records)
df_entity.to_csv(entity_csv_path, index=False)

print(f"Saved {len(entity_records)} entity-level predictions to {entity_csv_path} and {entity_jsonl_path}")
