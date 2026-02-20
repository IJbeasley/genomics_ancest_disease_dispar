#!/usr/bin/env python3
"""
Extract methods section from JATS XML files.
This script extracts all text content from sections marked with sec-type="methods".
"""

import xml.etree.ElementTree as ET
import argparse
import sys
from pathlib import Path


def extract_text_from_element(element, parent_tag=None):
    """
    Recursively extract all text content from an XML element,
    including nested elements, while preserving structure with line breaks.
    """
    text_parts = []
    current_tag = element.tag.split('}')[-1] if '}' in element.tag else element.tag
    
    # Check if this is a section with a title
    if current_tag == 'sec':
        # Look for label and title elements
        label_elem = None
        title_elem = None
        
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag == 'label':
                label_elem = child
            elif child_tag == 'title':
                title_elem = child
        
        # Add section header with period at the end
        if label_elem is not None or title_elem is not None:
            header_parts = []
            if label_elem is not None:
                # Recursively get all text from label
                label_text = extract_text_from_element(label_elem, current_tag)
                if label_text:
                    header_parts.append(label_text.strip())
            if title_elem is not None:
                # Recursively get all text from title (including nested elements)
                title_text = extract_text_from_element(title_elem, current_tag)
                if title_text:
                    header_parts.append(title_text.strip())
            
            if header_parts:
                text_parts.append(' '.join(header_parts) + '. ')
        
        # Process other children (except label and title which we already handled)
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag not in ['label', 'title']:
                child_text = extract_text_from_element(child, current_tag)
                if child_text:
                    text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Check if this is a paragraph - add spacing between paragraphs
    elif current_tag == 'p':
        # Get all text from the paragraph
        para_text_parts = []
        
        if element.text:
            para_text_parts.append(element.text.strip())
        
        for child in element:
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                para_text_parts.append(child_text)
            
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    para_text_parts.append(tail_text)
        
        # Join paragraph parts and normalize internal whitespace
        import re
        para_text = ' '.join(filter(None, para_text_parts))
        para_text = re.sub(r'\s+', ' ', para_text).strip()
        
        # Add paragraph with space after it
        if para_text:
            text_parts.append(para_text + ' ')
    
    else:
        # For non-section, non-paragraph elements, process normally
        # Get the element's own text
        if element.text:
            text_parts.append(element.text.strip())
        
        # Process all child elements recursively
        for child in element:
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Join all parts with spaces
    result = ' '.join(filter(None, text_parts))
    return result


def find_methods_section(root):
    """
    Find the methods section in a JATS XML document.
    Returns the methods section element or None if not found.
    
    Searches for methods section in multiple ways:
    1. sec-type="methods" attribute
    2. title text containing "methods" (case-insensitive)
    
    Skips methods sections inside abstracts (these are summaries, not full methods).
    """
    def is_inside_abstract(element):
        """Check if an element is inside an abstract tag."""
        parent = element
        while parent is not None:
            parent_tag = parent.tag.split('}')[-1] if '}' in parent.tag else parent.tag
            if parent_tag == 'abstract':
                return True
            # Move up to parent (need to traverse the tree)
            # Since we're iterating, we'll use a different approach
            break
        return False
    
    # First try: Look for sections with sec-type="methods" (not in abstract)
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            sec_type = sec.get('sec-type')
            if sec_type and sec_type.lower() == 'methods':
                # Check if this is inside an abstract by looking at ancestors
                # Get the path from root to this element
                for parent in root.iter():
                    if parent.tag.endswith('abstract'):
                        # Check if sec is a descendant of this abstract
                        for child in parent.iter():
                            if child == sec:
                                # This sec is inside an abstract, skip it
                                break
                        else:
                            continue
                        break
                else:
                    # Not in an abstract
                    return sec
    
    # Second try: Look for sections with title containing "methods" (not in abstract)
    # Common patterns: "Methods", "Materials and Methods", "Methods and Materials"
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            # Check if inside abstract first
            is_in_abstract = False
            for parent in root.iter():
                if parent.tag.endswith('abstract'):
                    for child in parent.iter():
                        if child == sec:
                            is_in_abstract = True
                            break
                    if is_in_abstract:
                        break
            
            if is_in_abstract:
                continue
            
            # Look for a title child element
            for child in sec:
                if child.tag.endswith('title'):
                    title_text = ''.join(child.itertext()).strip().lower()
                    # Check if title contains methods-related keywords
                    if ('method' in title_text and 
                        not title_text.startswith('result') and
                        not title_text.startswith('discussion') and
                        # Avoid subsections like "Statistical methods"
                        len(title_text.split()) <= 6):
                        return sec
                    break  # Only check first title
    
    return None


def extract_methods_section(xml_file):
    """
    Extract the methods section text from a JATS XML file.
    
    Args:
        xml_file: Path to the JATS XML file
        
    Returns:
        String containing the methods section text, or None if not found
    """
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()
        
        # Find the methods section
        methods_section = find_methods_section(root)
        
        if methods_section is None:
            # Check if methods are in supplementary materials
            supplementary_note = check_supplementary_methods(root)
            if supplementary_note:
                return supplementary_note
            return None
        
        # Extract all text from the methods section
        methods_text = extract_text_from_element(methods_section)
        
        # Final cleanup: strip trailing whitespace
        methods_text = methods_text.strip()
        
        return methods_text
        
    except ET.ParseError as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)
        return None
    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return None


def check_supplementary_methods(root):
    """
    Check if methods section is in supplementary materials (common in Cell Press journals).
    Returns a note if supplementary methods are found, None otherwise.
    """
    # Look for supplementary-material sections
    for elem in root.iter():
        tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
        if tag == 'supplementary-material' or (tag == 'sec' and elem.get('sec-type') == 'supplementary-material'):
            # Check all text in this element for methods references
            all_text = ' '.join(elem.itertext()).lower()
            if 'method' in all_text and ('supplemental' in all_text or 'supplementary' in all_text):
                # Found reference to supplementary methods
                # Try to extract the file name if available
                media_elem = elem.find('.//{*}media')
                if media_elem is not None:
                    href = media_elem.get('{http://www.w3.org/1999/xlink}href')
                    if href:
                        return f"NOTE: Methods section is in supplementary materials file: {href}\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
                
                return "NOTE: Methods section is in supplementary materials.\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
    
    return None


def main():
    parser = argparse.ArgumentParser(
        description='Extract methods section from JATS XML files'
    )
    parser.add_argument(
        'input_file',
        help='Path to the input JATS XML file'
    )
    parser.add_argument(
        '-o', '--output',
        help='Path to the output text file (if not specified, prints to stdout)'
    )
    
    args = parser.parse_args()
    
    # Extract methods section
    methods_text = extract_methods_section(args.input_file)
    
    if methods_text is None:
        print("No methods section found in the XML file.", file=sys.stderr)
        sys.exit(1)
    
    # Output the results
    if args.output:
        output_path = Path(args.output)
        output_path.write_text(methods_text, encoding='utf-8')
        print(f"Methods section extracted to: {args.output}")
    else:
        print(methods_text)


if __name__ == '__main__':
    main()
