#!/usr/bin/env python3
"""
Validate preprocessing quality of extracted methods text.
Quick checks to ensure text is ready for NLP/sentence tokenization.
"""

import sys
import re
from collections import Counter

def check_file(filepath):
    """Run all validation checks on a file."""
    
    with open(filepath, 'r', encoding='utf-8') as f:
        text = f.read()
    
    print("="*60)
    print(f"PREPROCESSING VALIDATION: {filepath}")
    print("="*60)
    print()
    
    # Track issues
    issues = []
    warnings = []
    
    # 1. Citation artifacts
    print("📚 CITATION CLEANUP:")
    empty_brackets = len(re.findall(r'\[\s*\]', text))
    empty_parens = len(re.findall(r'\(\s*\)', text))
    et_al_cites = len(re.findall(r'\([A-Z][a-zA-Z\s&,;.]+et al[,;\s.]*\)', text))
    number_brackets = len(re.findall(r'\[\s*\d+\s*(?:,\s*\d+\s*)*\]', text))
    
    print(f"   Empty brackets []: {empty_brackets}")
    print(f"   Empty parens (): {empty_parens}")
    print(f"   'et al' citations: {et_al_cites}")
    print(f"   Number brackets [1,2]: {number_brackets}")
    
    if empty_brackets > 0: issues.append(f"{empty_brackets} empty brackets")
    if empty_parens > 0: issues.append(f"{empty_parens} empty parentheses")
    if et_al_cites > 0: issues.append(f"{et_al_cites} et al citations")
    if number_brackets > 0: warnings.append(f"{number_brackets} numbered citations")
    print()
    
    # 2. Spacing issues
    print("📏 SPACING:")
    space_period = len(re.findall(r' \.', text))
    space_comma = len(re.findall(r' ,', text))
    space_semicolon = len(re.findall(r' ;', text))
    double_space = len(re.findall(r'  +', text))
    space_in_parens = len(re.findall(r'\(\s+|\s+\)', text))
    
    print(f"   Space before period ' .': {space_period}")
    print(f"   Space before comma ' ,': {space_comma}")
    print(f"   Space before semicolon ' ;': {space_semicolon}")
    print(f"   Multiple spaces: {double_space}")
    print(f"   Extra space in parens: {space_in_parens}")
    
    if space_period > 0: issues.append(f"{space_period} spaces before periods")
    if space_comma > 0: issues.append(f"{space_comma} spaces before commas")
    if space_semicolon > 0: issues.append(f"{space_semicolon} spaces before semicolons")
    if double_space > 50: warnings.append(f"{double_space} double spaces (unusual)")
    if space_in_parens > 0: issues.append(f"{space_in_parens} extra spaces in parentheses")
    print()
    
    # 3. Punctuation issues
    print("🔤 PUNCTUATION:")
    double_period = len(re.findall(r'\.\.+', text))
    double_comma = len(re.findall(r',,+', text))
    dash_comma = len(re.findall(r'[–—\-]\s*,', text))
    
    print(f"   Double periods '..': {double_period}")
    print(f"   Double commas ',,': {double_comma}")
    print(f"   Dash-comma '–,': {dash_comma}")
    
    if double_period > 0: issues.append(f"{double_period} double periods")
    if double_comma > 0: issues.append(f"{double_comma} double commas")
    if dash_comma > 0: issues.append(f"{dash_comma} dash-comma artifacts")
    print()
    
    # 4. LaTeX/markup noise
    print("🧹 LaTeX/MARKUP CLEANUP:")
    usepackage = len(re.findall(r'\\usepackage', text))
    documentclass = len(re.findall(r'\\documentclass', text))
    begin_doc = len(re.findall(r'\\begin\{document\}', text))
    
    print(f"   \\usepackage commands: {usepackage}")
    print(f"   \\documentclass: {documentclass}")
    print(f"   \\begin{{document}}: {begin_doc}")
    
    if usepackage > 0: issues.append(f"{usepackage} LaTeX usepackage commands")
    if documentclass > 0: issues.append(f"{documentclass} LaTeX documentclass")
    if begin_doc > 0: issues.append(f"{begin_doc} LaTeX begin commands")
    print()
    
    # 5. Section numbers
    print("🔢 SECTION NUMBERING:")
    section_nums = len(re.findall(r'(?:^|\. )(\d+\.)+\d*\s+[A-Z]', text))
    
    print(f"   Section numbers (e.g., '2.1 Methods'): {section_nums}")
    
    if section_nums > 0: warnings.append(f"{section_nums} section numbers found")
    print()
    
    # 6. Text statistics
    print("📊 STATISTICS:")
    word_count = len(text.split())
    sentence_count = len(re.findall(r'[.!?]+\s+', text))
    avg_sentence_len = word_count / max(sentence_count, 1)
    
    print(f"   Total words: {word_count:,}")
    print(f"   Estimated sentences: {sentence_count:,}")
    print(f"   Avg words/sentence: {avg_sentence_len:.1f}")
    print()
    
    # 7. Sample sentences
    print("📝 SAMPLE (first 3 sentences):")
    sentences = re.split(r'[.!?]+\s+', text)[:3]
    for i, sent in enumerate(sentences, 1):
        preview = sent[:100] + "..." if len(sent) > 100 else sent
        print(f"   {i}. {preview}")
    print()
    
    # Final verdict
    print("="*60)
    print("🏁 VERDICT:")
    print("="*60)
    
    if issues:
        print("❌ ISSUES FOUND:")
        for issue in issues:
            print(f"   • {issue}")
        print("\n⚠️  Text needs additional cleaning!")
        return False
    elif warnings:
        print("⚠️  WARNINGS:")
        for warning in warnings:
            print(f"   • {warning}")
        print("\n✅ Text is acceptable but check warnings")
        return True
    else:
        print("✅ PASSED - Text is clean and ready for NLP!")
        return True

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python validate_preprocessing.py <methods_file.txt>")
        sys.exit(1)
    
    filepath = sys.argv[1]
    success = check_file(filepath)
    sys.exit(0 if success else 1)
