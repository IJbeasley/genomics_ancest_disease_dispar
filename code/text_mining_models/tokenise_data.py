# Tokenise data for training a language model
# Exposes `get_tokenized_datasets`, which tokenizes train/val Datasets and
# aligns Doccano-style character-span labels to token-level BIO tags.


def tokenize_dataset(dataset, tokenizer, label2id, id2label):
    """Tokenize a single HuggingFace Dataset and align char-span labels to BIO tags.

    Works for unlabeled data too: if an example has an empty `label` list, all
    non-special tokens get label 'O' and special tokens get -100.

    Args:
        dataset:   HuggingFace Dataset with "text" and "label" columns, where
                   "label" is a list of [start, end, tag] character spans.
        tokenizer: A *fast* HuggingFace tokenizer (required for offset_mapping).
        label2id:  Dict mapping BIO label strings to integer IDs.
        id2label:  Dict mapping integer IDs back to BIO label strings (unused
                   here but accepted for API symmetry).

    Returns:
        Tokenized HuggingFace Dataset ready for Trainer.
    """
    def tokenize_and_align_label(batch):
        tokenized_inputs = tokenizer(
            batch["text"],
            truncation=True,
            padding="max_length",
            max_length=256,
            return_offsets_mapping=True,
        )

        all_label = []

        for i, spans in enumerate(batch["label"]):
            aligned_labels = []
            offsets = tokenized_inputs["offset_mapping"][i]

            for (token_start, token_end) in offsets:
                # Special tokens have (0, 0) offsets
                if token_start == token_end:
                    aligned_labels.append(-100)
                    continue

                token_label = "O"

                for span_start, span_end, span_tag in spans:
                    span_start = int(span_start)
                    span_end = int(span_end)
                    if token_end > span_start and token_start < span_end:
                        prefix = "B-" if token_start == span_start else "I-"
                        candidate = f"{prefix}{span_tag}"
                        if candidate not in label2id:
                            # unknown tag in input — either skip or raise
                            token_label = "O"
                        else:
                            token_label = candidate
                        break
                aligned_labels.append(label2id[token_label])

            all_label.append(aligned_labels)

        tokenized_inputs["labels"] = all_label
        tokenized_inputs.pop("offset_mapping")
        return tokenized_inputs

    return dataset.map(
        tokenize_and_align_label,
        batched=True,
        remove_columns=dataset.column_names,
    )


def get_tokenized_datasets(train_dataset, val_dataset, tokenizer, label2id, id2label):
    """Tokenize train/val datasets and align character-span labels to token-level BIO tags.

    Thin wrapper around `tokenize_dataset` that also prints token-level label
    count sanity checks for both splits.
    """
    tokenized_train = tokenize_dataset(train_dataset, tokenizer, label2id, id2label)
    tokenized_val = tokenize_dataset(val_dataset, tokenizer, label2id, id2label)

    # Token-level label count sanity check
    def count_token_labels(tokenized_dataset):
        counts = {label: 0 for label in label2id}
        #counts = {"B-COHORT": 0, "I-COHORT": 0, "O": 0}
        for example in tokenized_dataset:
            for l in example["labels"]:
                if l != -100:
                    counts[id2label[l]] += 1
        return counts

    train_token_counts = count_token_labels(tokenized_train)
    val_token_counts = count_token_labels(tokenized_val)

    print("\nToken-level label counts (training):  ", train_token_counts)
    print("Token-level label counts (validation):", val_token_counts, "\n")

    return tokenized_train, tokenized_val
