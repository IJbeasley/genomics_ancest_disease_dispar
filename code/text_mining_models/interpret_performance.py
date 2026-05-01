import pandas as pd
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
import matplotlib.pyplot as plt

df = pd.read_csv("output/text_mining_predictions/validation_predictions.csv")

# Exclude padding rows
df = df[df["true_label"] != "PAD"]

labels = sorted(set(df["true_label"].unique()) | set(df["pred_label"].unique()))

cm = confusion_matrix(df["true_label"], df["pred_label"], labels=labels)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=labels)
disp.plot(xticks_rotation=45)
plt.tight_layout()
plt.savefig("confusion_matrix.png", dpi=150)
