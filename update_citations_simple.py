#!/usr/bin/env python3
"""
Simple Citation Key Updater

This script updates BibTeX citation keys to uppercase and removes underscores,
then updates their usages in \furtherreading{} commands in LaTeX files.
"""

import os
import re
import glob

def normalize_citation_key(key):
    """Convert citation key to uppercase and remove underscores."""
    return key.replace('_', '').upper()

def find_bibtex_files():
    """Find all references.bib files in slides folders."""
    pattern = "slides/*/references.bib"
    return glob.glob(pattern)

def update_bibtex_file(filepath):
    """Update citation keys in a BibTeX file."""
    print(f"Processing: {filepath}")
    
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find all citation keys and create mappings
    pattern = r'@(\w+)\s*\{\s*([^,\s]+)\s*,'
    mappings = {}
    
    for match in re.finditer(pattern, content):
        old_key = match.group(2)
        new_key = normalize_citation_key(old_key)
        if old_key != new_key:
            mappings[old_key] = new_key
            print(f"  {old_key} → {new_key}")
    
    if not mappings:
        print("  No changes needed")
        return {}
    
    # Update the content
    updated_content = content
    for old_key, new_key in mappings.items():
        # Replace in BibTeX entries
        pattern = r'(@\w+\s*\{\s*)(' + re.escape(old_key) + r')(\s*,)'
        replacement = r'\1' + new_key + r'\3'
        updated_content = re.sub(pattern, replacement, updated_content)
    
    # Write back to file
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(updated_content)
    
    print(f"  Updated {len(mappings)} citation keys")
    return mappings

def find_tex_files_in_folder(folder):
    """Find all .tex files in a specific folder."""
    pattern = os.path.join(folder, "*.tex")
    return glob.glob(pattern)

def update_tex_file(filepath, mappings):
    """Update citation keys in a LaTeX file."""
    if not mappings:
        return 0
    
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    updates_made = 0
    updated_content = content
    
    for old_key, new_key in mappings.items():
        # Update \furtherreading{old_key}
        pattern = r'(\\furtherreading\{)' + re.escape(old_key) + r'(\})'
        replacement = r'\1' + new_key + r'\2'
        
        matches = len(re.findall(pattern, updated_content))
        if matches > 0:
            updated_content = re.sub(pattern, replacement, updated_content)
            updates_made += matches
    
    if updates_made > 0:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(updated_content)
        
        relative_path = os.path.relpath(filepath)
        print(f"    Updated {relative_path}: {updates_made} citations")
    
    return updates_made

def main():
    print("Simple Citation Key Updater")
    print("=" * 40)
    
    # Find all BibTeX files
    bibtex_files = find_bibtex_files()
    
    if not bibtex_files:
        print("No BibTeX files found in slides folders")
        return
    
    total_mappings = 0
    total_tex_updates = 0
    
    # Process each BibTeX file and its corresponding LaTeX files
    for bibtex_file in bibtex_files:
        # Update BibTeX file
        mappings = update_bibtex_file(bibtex_file)
        total_mappings += len(mappings)
        
        if mappings:
            # Find corresponding folder
            folder = os.path.dirname(bibtex_file)
            
            # Find and update LaTeX files in the same folder
            tex_files = find_tex_files_in_folder(folder)
            print(f"  Updating {len(tex_files)} LaTeX files in {folder}")
            
            for tex_file in tex_files:
                updates = update_tex_file(tex_file, mappings)
                total_tex_updates += updates
        
        print()
    
    print("Summary:")
    print(f"- BibTeX files processed: {len(bibtex_files)}")
    print(f"- Total citation mappings: {total_mappings}")
    print(f"- Total LaTeX updates: {total_tex_updates}")

if __name__ == "__main__":
    main()
