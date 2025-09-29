#!/usr/bin/env python3
"""
Fix remaining path issues in LaTeX exercise files after restructuring
"""

import os
import re
from pathlib import Path

class PathFixer:
    def __init__(self, base_path):
        self.base_path = Path(base_path)
        self.fixes_applied = []
        
    def find_and_fix_paths(self):
        """Find and fix all path issues in exercise files."""
        exercises_path = self.base_path / "exercises"
        
        if not exercises_path.exists():
            print(f"❌ Exercises directory not found: {exercises_path}")
            return
            
        # Process all .tex files (excluding ex_tex for now)
        for tex_file in exercises_path.rglob("*.tex"):
            if 'ex_tex' in str(tex_file):
                continue
                
            self.fix_file_paths(tex_file)
    
    def fix_file_paths(self, tex_file):
        """Fix paths in a specific tex file."""
        print(f"🔍 Checking: {tex_file.relative_to(self.base_path)}")
        
        try:
            with open(tex_file, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
            original_content = content
            
            # Fix specific known issues based on the compilation report
            content = self.fix_shapley_references(content, tex_file)
            content = self.fix_feature_importance_references(content, tex_file)
            content = self.fix_feature_effects_references(content, tex_file)
            
            if content != original_content:
                with open(tex_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                print(f"  ✅ Fixed paths in {tex_file.name}")
                self.fixes_applied.append(tex_file)
            else:
                print(f"  ✓ No path fixes needed")
                
        except Exception as e:
            print(f"  ❌ Error processing {tex_file}: {e}")
    
    def fix_shapley_references(self, content, tex_file):
        """Fix references in shapley files."""
        if 'shapley' not in str(tex_file):
            return content
            
        # Fix the specific missing reference
        fixes = {
            'ex_tex/hw_4_4_axioms_proofs': 'ex_tex/hw_shapley_4_axioms_proofs.tex',
            'ex_tex/hw_sol_4_4_axioms_proofs': 'ex_tex/hw_sol_shapley_4_axioms_proofs.tex',
        }
        
        for old_ref, new_ref in fixes.items():
            if old_ref in content:
                content = content.replace(old_ref, new_ref)
                print(f"    Fixed: {old_ref} -> {new_ref}")
                
        return content
    
    def fix_feature_importance_references(self, content, tex_file):
        """Fix references in feature importance files."""
        if 'feature_importance' not in str(tex_file):
            return content
            
        # Fix the specific missing reference
        fixes = {
            'ex_tex/sol_fi_2.tex': 'ex_tex/hw_sol_fi_2_PLACEHOLDER.tex',  # This file might not exist
        }
        
        for old_ref, new_ref in fixes.items():
            if old_ref in content:
                # Check if the referenced file actually exists
                ref_path = tex_file.parent / new_ref
                if not ref_path.exists():
                    # Comment out the reference instead
                    content = content.replace(f'\\input{{{old_ref}}}', f'%\\input{{{old_ref}}} % File not found - commented out')
                    print(f"    Commented out missing reference: {old_ref}")
                else:
                    content = content.replace(old_ref, new_ref)
                    print(f"    Fixed: {old_ref} -> {new_ref}")
                
        return content
    
    def fix_feature_effects_references(self, content, tex_file):
        """Fix references in feature effects files."""
        if 'feature_effects' not in str(tex_file):
            return content
            
        # Fix references to old numbered folders
        fixes = {
            'exercises/04_feature_effects/ex_tex/ic_X_fe_trees': 'ex_tex/ic_fe_X_fe_trees.tex',
            'exercises/04_feature_effects/ex_tex/ic_sol_X_fe_trees': 'ex_tex/ic_sol_fe_X_fe_trees.tex',
        }
        
        for old_ref, new_ref in fixes.items():
            if old_ref in content:
                content = content.replace(old_ref, new_ref)
                print(f"    Fixed: {old_ref} -> {new_ref}")
                
        return content
    
    def generate_report(self):
        """Generate a report of all fixes applied."""
        print("\n" + "=" * 80)
        print("PATH FIXING REPORT")
        print("=" * 80)
        print(f"📊 Total files processed with fixes: {len(self.fixes_applied)}")
        
        if self.fixes_applied:
            print("\n✅ FILES WITH PATH FIXES:")
            for file_path in self.fixes_applied:
                print(f"  • {file_path.relative_to(self.base_path)}")
        else:
            print("\n✅ No path fixes were needed!")
            
        print(f"\n📄 Report completed!")

def main():
    base_path = Path.cwd()
    fixer = PathFixer(base_path)
    
    print("🔧 Starting path fixing process...")
    print(f"📁 Base directory: {base_path}")
    print("")
    
    fixer.find_and_fix_paths()
    fixer.generate_report()

if __name__ == "__main__":
    main()