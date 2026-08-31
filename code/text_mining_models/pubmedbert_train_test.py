from datasets import Dataset, Features, Value, Sequence
from transformers import AutoTokenizer, AutoModelForTokenClassification, AutoConfig, TrainingArguments, Trainer, DataCollatorForTokenClassification, EarlyStoppingCallback
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
import inspect
import matplotlib.pyplot as plt
import itertools, json, copy

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
    default="COHORT",
    help="Comma-separated list of entity types for NER (default: COHORT)"
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
    "--- PubMedBERT versions: ---" \
    "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext, " \
    "microsoft/BiomedNLP-BiomedBERT-large-uncased-abstract, " \
    "microsoft/BiomedNLP-BiomedBERT-base-uncased-abstract" \
    "--- bioformer versions: ---"
    "bioformers/bioformer-8L" \
    "bioformers/bioformer-16L" \
    "--- BioBERT options:  https://huggingface.co/collections/dmis-lab/biobert ---"
    "dmis-lab/biobert-large-cased-v1.1" \
    " dmis-lab/biobert-base-cased-v1.1"
)
parser.add_argument(
    "--test_path",
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
parser.add_argument(
    "--grid_search",
    action="store_true",
    help=(
        "Whether to perform a hyperparameter grid search"
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

       

############## Split train/validation ##############

       from sklearn.model_selection import StratifiedGroupKFold
       import numpy as np

       # Create binary stratification target: whether sample has any COHORT labels
       stratify_target = np.array([1 if any(label[2] == "COHORT" for label in example.get("label", [])) else 0
                           for example in train_data])
       groups = np.array([example.get("pubmed_id", -1) for example in train_data])

       # Create binary stratification target: whether sample has any entity labels (any type)
       # stratify_target = np.array([1 if len(example.get("label", [])) > 0 else 0 
       #                      for example in train_data])
       # groups = np.array([example.get("pubmed_id", -1) for example in train_data])

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

# Count spans per entity (entity-agnostic)
def count_entity_spans(dataset):
    counts = {t: 0 for t in entity_types}
    for example in dataset:
        for span in example.get("label", []):
            if len(span) >= 3:
                tag = span[2]
                counts[tag] = counts.get(tag, 0) + 1
    return counts

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

   # The fine-tuned checkpoint may have been trained with a wider set of
   # entity types than --entity_types declares (it's a model artifact, not a
   # user-supplied schema). Rebuild label_list / id2label / label2id / num_labels
   # from the saved config so decoding lines up with the model's output head.
   saved_id2label = model.config.id2label or {}
   # config.id2label keys are strings when loaded from JSON; normalize to ints
   id2label   = {int(k): v for k, v in saved_id2label.items()}
   label_list = [id2label[i] for i in range(len(id2label))]
   label2id   = {label: i for i, label in id2label.items()}
   num_labels = len(label_list)


if not args_parsed.skip_training:
    # Only overwrite when we built id2label/label2id from --entity_types; in
    # skip-training mode we just sourced them FROM the loaded config above.
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


# 8. Data collator + Trainer (built in both branches)
data_collator = DataCollatorForTokenClassification(tokenizer)

# Default TrainingArguments kwargs. Kept as a dict so grid-search trials can
# build their own TrainingArguments by overriding individual keys, without
# trying to deep-copy/mutate a TrainingArguments instance (which is a frozen-ish
# dataclass and doesn't support item assignment).
default_ta_kwargs = dict(
    output_dir                 = "pubmedbert-cohort-ner",
    learning_rate              = 5e-5,  # try: 1e-5, 3e-5, 5e-5
    per_device_train_batch_size= 16,     # try: 16, 32
    per_device_eval_batch_size = 16,
    num_train_epochs           = 5,      # try: 3, 5, 10
    weight_decay = 0.2,
    logging_steps              = 100,
    seed                       = args_parsed.seed,
    eval_strategy              = "epoch",
    eval_delay                 = 0, 
    # Save a checkpoint at the end of every epoch (must match eval_strategy
    # for load_best_model_at_end to work).
    save_strategy              = "epoch",
    # Keep only the 2 most recent checkpoints on disk so save_strategy="epoch"
    # doesn't fill up the workspace; the best-by-F1 checkpoint is always kept.
    save_total_limit           = 2,
    # After training, restore the checkpoint with the best validation F1.
    load_best_model_at_end     = True,
    metric_for_best_model      = "eval_f1",
    greater_is_better          = True,
)

ta_kwargs = copy.deepcopy(default_ta_kwargs)
if args_parsed.skip_training:
    # No training loop is going to run, so the epoch-based eval/save strategies
    # (and load_best_model_at_end / early stopping) are meaningless and Trainer
    # rejects them when there's no eval_dataset. Disable them.
    ta_kwargs["eval_strategy"]          = "no"
    ta_kwargs["save_strategy"]          = "no"
    ta_kwargs["load_best_model_at_end"] = False
    ta_kwargs.pop("metric_for_best_model", None)
    ta_kwargs.pop("greater_is_better", None)

training_args = TrainingArguments(**ta_kwargs)

trainer_kwargs = dict(
    model=model,
    args=training_args,
    data_collator=data_collator,
    compute_metrics=compute_metrics,
)
if not args_parsed.skip_training:
    trainer_kwargs["train_dataset"] = tokenized_train
    trainer_kwargs["eval_dataset"] = tokenized_val
    # EarlyStopping requires load_best_model_at_end=True, which we disable above
    # in skip-training mode — so only attach it when actually training.
    trainer_kwargs["callbacks"] = [EarlyStoppingCallback(early_stopping_patience=2)]

trainer = Trainer(**trainer_kwargs)


if not args_parsed.skip_training:
    # Hyperparameter tuning notes:
    #   learning rate    (1e-5, 3e-5, 5e-5)
    #   sequence length  (128, 256, 512)   <-- needs re-tokenisation; not swept here
    #   batch size       (16, 32)
    #   dropout rate     (0.1, 0.2)
    # The model that achieves the best F1 on the validation set is kept.

    if args_parsed.grid_search:
        # The hyperparameters you want to sweep.
        # NB: max_length lives in tokenise_data.py — to sweep it you'd have to
        # parametrise tokenize_dataset(..., max_length=...) and re-tokenise per trial.
        param_grid = {
            "learning_rate":               [1e-5, 3e-5, 5e-5],
            "per_device_train_batch_size": [16, 32],
            "num_train_epochs":            [3, 5],
            "hidden_dropout_prob":         [0.1, 0.2],
        }
        keys, values = zip(*param_grid.items())
        trials = [dict(zip(keys, combo)) for combo in itertools.product(*values)]
        print(f"\n=== Grid search: {len(trials)} trials ===")

        best = {"score": -float("inf"), "config": None, "trainer": None}
        results = []

        for trial_idx, hp in enumerate(trials):
            print(f"\n========== Trial {trial_idx + 1}/{len(trials)}: {hp} ==========")

            # Fresh TrainingArguments per trial (start from defaults, override the
            # swept fields). copy.deepcopy on the dict — NOT on TrainingArguments.
            trial_ta_kwargs = copy.deepcopy(default_ta_kwargs)
            trial_ta_kwargs["learning_rate"]               = hp["learning_rate"]
            trial_ta_kwargs["per_device_train_batch_size"] = hp["per_device_train_batch_size"]
            trial_ta_kwargs["num_train_epochs"]            = hp["num_train_epochs"]
            trial_ta_kwargs["output_dir"]                  = f"pubmedbert-grid/trial_{trial_idx}"
            trial_args = TrainingArguments(**trial_ta_kwargs)
            if getattr(trial_args, "eval_delay", 0) is None:
                trial_args.eval_delay = 0

            # Fresh model + config so each trial restarts from the pre-trained
            # checkpoint and uses the trial's dropout rate.
            trial_config = AutoConfig.from_pretrained(
                model_name,
                num_labels=num_labels,
                hidden_dropout_prob=hp["hidden_dropout_prob"],
                attention_probs_dropout_prob=hp["hidden_dropout_prob"],
            )
            trial_model = AutoModelForTokenClassification.from_pretrained(
                model_name, config=trial_config
            )
            trial_model.config.id2label = id2label
            trial_model.config.label2id = label2id

            trial_trainer = Trainer(
                model=trial_model,
                args=trial_args,
                train_dataset=tokenized_train,
                eval_dataset=tokenized_val,
                data_collator=data_collator,
                compute_metrics=compute_metrics,
                callbacks=[EarlyStoppingCallback(early_stopping_patience=2)],
            )
            trial_trainer.train()
            metrics = trial_trainer.evaluate()

            score = metrics["eval_f1"]   # use -metrics["eval_loss"] to optimise loss instead
            results.append({
                "trial":          trial_idx,
                **hp,
                "eval_loss":      metrics.get("eval_loss"),
                "eval_f1":        metrics.get("eval_f1"),
                "eval_precision": metrics.get("eval_precision"),
                "eval_recall":    metrics.get("eval_recall"),
                "eval_accuracy":  metrics.get("eval_accuracy"),
            })

            # Persist the leaderboard incrementally so you can inspect progress
            # mid-sweep and don't lose anything if a later trial blows up.
            os.makedirs(args_parsed.output_path, exist_ok=True)
            leaderboard_path = f"{args_parsed.output_path}/grid_search_results.csv"
            pd.DataFrame(results).sort_values("eval_f1", ascending=False).to_csv(
                leaderboard_path, index=False
            )

            if score > best["score"]:
                best = {"score": score, "config": hp, "trainer": trial_trainer}
                print(f"  *** New best: F1={score:.4f} ***")

        print(f"\n=== Grid search complete ===")
        print(f"Leaderboard: {leaderboard_path}")
        print(f"Best config: {best['config']}  eval_f1={best['score']:.4f}")

        # Promote the best trial's trainer/model so the rest of the script
        # (loss-plot logic, save_model, eval, prediction CSVs) operates on the
        # winning model.
        trainer = best["trainer"]
        model = trainer.model
    else:
        trainer.train()
    
    # Create and save training loss plot
    history = trainer.state.log_history
    
    # Extract training loss (only entries with 'loss' key, excluding eval_loss entries)
    train_steps = []
    train_loss = []
    eval_steps = []
    eval_loss = []
    
    for log in history:
        if "loss" in log and "eval_loss" not in log:
            train_steps.append(log.get("step", len(train_steps)))
            train_loss.append(log["loss"])
        if "eval_loss" in log:
            eval_steps.append(log.get("step", len(eval_steps)))
            eval_loss.append(log["eval_loss"])
    
    # Create plot
    plt.figure(figsize=(12, 6))
    if train_loss:
        plt.plot(train_steps, train_loss, marker='o', linewidth=2, label='Training Loss', alpha=0.8)
    if eval_loss:
        plt.plot(eval_steps, eval_loss, marker='s', linewidth=2, label='Validation Loss', alpha=0.8)
    
    plt.xlabel('Training Step', fontsize=12)
    plt.ylabel('Loss', fontsize=12)
    plt.title('Training and Validation Loss Over Steps', fontsize=14)
    plt.legend(fontsize=11)
    plt.grid(True, alpha=0.3)
    
    loss_plot_path = f"{args_parsed.output_path}/training_loss.png"
    os.makedirs(args_parsed.output_path, exist_ok=True)
    plt.savefig(loss_plot_path, dpi=150, bbox_inches='tight')
    print(f"\n=== Saved training loss plot to {loss_plot_path} ===")
    print(f"    Logged {len(train_loss)} training steps and {len(eval_loss)} validation steps")
    plt.close()

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

# id2label-based lookup with an "O" fallback so any unexpected label ID
# (e.g. left over from a stale tokenized cache built with a wider entity set)
# becomes "O" instead of raising IndexError.
_id2label_safe = {i: label for i, label in enumerate(label_list)}
def _decode(stream_id):
    return _id2label_safe.get(int(stream_id), "O")

true_predictions = [
    [_decode(p) for (p, l) in zip(pred, lab) if l != -100]
    for pred, lab in zip(pred_labels, labels)
]
true_label = [
    [_decode(l) for (p, l) in zip(pred, lab) if l != -100]
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
    # NB: use a separate variable name for training labels so we don't clobber
    # the validation `labels` array, which is still referenced downstream.
    train_predictions, train_labels, _ = trainer.predict(tokenized_train)
    train_probs = scipy.special.softmax(train_predictions, axis=2)
    train_pred_label_ids = np.argmax(train_predictions, axis=2)

    # Decode BIO labels for the TRAINING set (do NOT reuse `true_predictions`/
    # `true_label` from the validation block — those have validation length and
    # would raise IndexError when iterated over the larger training set).
    #
    # Use _decode (id2label.get(..., "O")) rather than label_list[l] so any
    # unexpected label ID (e.g. left over from a stale tokenized cache built
    # with a wider set of entity types) gracefully falls back to "O" instead
    # of raising IndexError: list index out of range.
    train_true_predictions = [
        [_decode(p) for (p, l) in zip(pred, lab) if l != -100]
        for pred, lab in zip(train_pred_label_ids, train_labels)
    ]
    train_true_label = [
        [_decode(l) for (p, l) in zip(pred, lab) if l != -100]
        for pred, lab in zip(train_pred_label_ids, train_labels)
    ]

    # One-time diagnostic: warn if the training labels contain any IDs we
    # don't recognise (i.e. anything other than -100 or a key of _id2label_safe).
    _unknown = sorted({
        int(l) for lab in train_labels for l in lab
        if int(l) != -100 and int(l) not in _id2label_safe
    })
    if _unknown:
        warnings.warn(
            "Training labels contain unexpected ID(s) "
            f"{_unknown} not present in id2label={_id2label_safe}. "
            "These are being treated as 'O'. This usually means a stale HF "
            "datasets cache is being reused — try clearing "
            "~/.cache/huggingface/datasets and re-running, or re-tokenising "
            "the training set from scratch."
        )

    train_per_example_conf = []
    for i, (prob_seq, lab_seq) in enumerate(zip(train_probs, train_labels)):
        entity_mask = (lab_seq != -100) & (lab_seq != 0)
        if entity_mask.any():
            conf = prob_seq[entity_mask].max(axis=1).mean()
        else:
            conf = 1.0
        train_per_example_conf.append(conf)

    # save train confidences to a CSV
    n_train = len(train_per_example_conf)
    df_train_conf = pd.DataFrame({
        "text":       [train_data[i]["text"]            for i in range(n_train)],
        "pred_label": [train_true_predictions[i]        for i in range(n_train)],
        "true_label": [train_true_label[i]              for i in range(n_train)],
        "confidence": train_per_example_conf,
    })

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


# === Entity-level confusion matrix ===========================================
# Token-level CMs are dominated by 'O' tokens and don't tell you whether an
# entity was correctly recognised. We build an entity-level CM instead:
#   - Walk each BIO sequence to extract (start, end, type) spans.
#   - Match predicted spans to true spans by EXACT (start, end) position
#     (this matches seqeval's "strict" scoring convention).
#   - For each example, emit (true_type, pred_type) pairs:
#       both spans match span-wise        -> (true_type, pred_type)
#                                            (these are TPs when types agree,
#                                             type-confusions when they don't)
#       true span has no matching pred    -> (true_type, "O")     [FN]
#       pred span has no matching true    -> ("O", pred_type)     [FP]
# Rows = true entity type, cols = predicted entity type.
from sklearn.metrics import confusion_matrix


def _bio_to_spans(tags):
    """Convert a BIO tag sequence into a list of (start, end_exclusive, type) spans."""
    spans = []
    cur_start, cur_type = None, None
    for i, tag in enumerate(tags):
        if tag == "O" or tag.startswith("B-"):
            if cur_start is not None:
                spans.append((cur_start, i, cur_type))
                cur_start, cur_type = None, None
            if tag.startswith("B-"):
                cur_start, cur_type = i, tag[2:]
        elif tag.startswith("I-"):
            t = tag[2:]
            if cur_type == t and cur_start is not None:
                pass  # extend current span
            else:
                # Dangling I-: treat as the start of a new span
                if cur_start is not None:
                    spans.append((cur_start, i, cur_type))
                cur_start, cur_type = i, t
    if cur_start is not None:
        spans.append((cur_start, len(tags), cur_type))
    return spans


entity_true, entity_pred = [], []
for true_tags, pred_tags in zip(true_label, true_predictions):
    true_spans = {(s, e): t for s, e, t in _bio_to_spans(true_tags)}
    pred_spans = {(s, e): t for s, e, t in _bio_to_spans(pred_tags)}

    for span in set(true_spans) | set(pred_spans):
        entity_true.append(true_spans.get(span, "O"))   # "O" if predicted span has no gold match
        entity_pred.append(pred_spans.get(span, "O"))   # "O" if gold span has no predicted match

# Order the axes: real entity types first (alphabetical), then "O" last so the
# FP / FN row+column live in the bottom-right corner of the matrix.
entity_axes = sorted(
    {lab for lab in entity_true + entity_pred if lab != "O"}
) + ["O"]

cm = confusion_matrix(entity_true, entity_pred, labels=entity_axes)
cm_df = pd.DataFrame(cm, index=entity_axes, columns=entity_axes)
cm_df.index.name = "true \\ pred"

print("\n=== Confusion Matrix (entity-level, strict span match) ===")
print(cm_df)
print(
    "Reading the matrix:\n"
    "  diagonal cells (excluding the 'O' row/col) are correctly recognised entities (TP)\n"
    "  off-diagonal cells (excluding 'O') are TYPE confusions on a correctly located span\n"
    "  row 'O', col X      = spurious predictions of type X (false positives)\n"
    "  row X, col 'O'      = missed gold entities of type X (false negatives)\n"
    "  cell ('O','O')      = 0 by construction (we don't count 'no entity in either')"
)

cm_csv_path = f"{args_parsed.output_path}/{output_prefix}_entity_confusion_matrix.csv"
os.makedirs(args_parsed.output_path, exist_ok=True)
cm_df.to_csv(cm_csv_path)
print(f"=== Saved entity-level confusion matrix to {cm_csv_path} ===")

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


# === Save PubMedBERT annotations in Doccano JSONL format =====================
# Mirrors the input file's schema (text, pubmed_id, date, country,
# gwas_cat_cohort_label, label) but replaces `label` with the model's predicted
# character-level spans [[start, end, TAG], ...]. Spans are reconstructed by
# re-tokenising each example with offset_mapping and walking BIO tags — same
# logic used above for entity-level predictions.
annotations_jsonl_path = f"{args_parsed.output_path}/pubmedbert_annotations.jsonl"

with open(annotations_jsonl_path, "w", encoding="utf-8") as f:
    for i in range(len(eval_raw_data)):
        original_text = eval_raw_data[i]["text"]

        # Re-extract predicted spans for this example (true_spans is unused here).
        _, pred_spans = extract_entity_spans(
            original_text,
            tokenizer,
            labels[i],
            pred_labels[i],
            id2label,
        )

        # extract_entity_spans returns (surface_text, label, start_char, end_char);
        # Doccano format expects [start, end, TAG]. Match the input file's
        # stringified-offsets convention.
        pred_label_field = [
            [str(s[2]), str(s[3]), s[1]] for s in pred_spans
        ]

        record = {
            "text":                  original_text,
            "pubmed_id":             eval_raw_data[i].get("pubmed_id", ""),
            "date":                  eval_raw_data[i].get("date", ""),
            "country":               eval_raw_data[i].get("country", ""),
            "gwas_cat_cohort_label": eval_raw_data[i].get("gwas_cat_cohort_label", ""),
            "label":                 pred_label_field,
        }
        f.write(json.dumps(record, ensure_ascii=False) + "\n")

print(f"Saved {len(eval_raw_data)} PubMedBERT annotations (Doccano format) to {annotations_jsonl_path}")
