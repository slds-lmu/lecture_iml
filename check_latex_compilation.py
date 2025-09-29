#!/usr/bin/env python3
"""
LaTeX Compilation Checker for Exercise and Slide Files

This script recursively scans the exercises and slides directories for .tex files,
attempts to compile each one, and reports compilation failures with details.
"""

import os
import subprocess
import sys
import re
from pathlib import Path
import tempfile
import shutil
from datetime import datetime

class LatexChecker:
    def __init__(self, base_path):
        self.base_path = Path(base_path)
        self.failures = []
        self.successes = []
        self.skipped = []
        
    def find_tex_files(self):
        """Find all .tex files in the exercises and slides directories."""
        tex_files = []
        
        # Check exercises directory
        exercises_path = self.base_path / "exercises"
        if exercises_path.exists():
            # Find all .tex files recursively, but skip ex_tex directories entirely
            for tex_file in exercises_path.rglob("*.tex"):
                # Skip temporary files and files in specific subdirectories we want to ignore
                if (tex_file.name.startswith('.') or 
                    tex_file.name.endswith('.aux') or
                    tex_file.name.endswith('.log') or
                    'summer-school' in str(tex_file) or
                    'attic' in str(tex_file) or
                    'ex_tex' in str(tex_file)):  # Skip all ex_tex files entirely
                    continue
                tex_files.append(tex_file)
        else:
            print(f"❌ Exercises directory not found: {exercises_path}")
        
        # Check slides directory
        slides_path = self.base_path / "slides"
        if slides_path.exists():
            # Find all .tex files recursively in slides
            for tex_file in slides_path.rglob("*.tex"):
                # Skip temporary files and files in specific subdirectories we want to ignore
                if (tex_file.name.startswith('.') or 
                    tex_file.name.endswith('.aux') or
                    tex_file.name.endswith('.log') or
                    'summer-school' in str(tex_file) or
                    'attic' in str(tex_file)):
                    continue
                tex_files.append(tex_file)
        else:
            print(f"⚠️  Slides directory not found: {slides_path}")
            
        return sorted(tex_files)
    
    def extract_dependencies(self, tex_file):
        """Extract file dependencies from a .tex file."""
        dependencies = []
        try:
            with open(tex_file, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
            # Find \input{}, \include{}, \includegraphics{} commands
            patterns = [
                r'\\input\{([^}]+)\}',
                r'\\include\{([^}]+)\}', 
                r'\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}',
                r'\\bibliography\{([^}]+)\}'
            ]
            
            for pattern in patterns:
                matches = re.findall(pattern, content)
                for match in matches:
                    # Resolve relative paths
                    dep_path = tex_file.parent / match
                    dependencies.append((match, dep_path))
                    
        except Exception as e:
            print(f"⚠️  Error reading {tex_file}: {e}")
            
        return dependencies
    
    def check_dependencies(self, tex_file):
        """Check if all dependencies of a .tex file exist."""
        missing_deps = []
        dependencies = self.extract_dependencies(tex_file)
        
        for orig_path, resolved_path in dependencies:
            # Check various extensions for input files
            possible_paths = [
                resolved_path,
                resolved_path.with_suffix('.tex'),
                resolved_path.with_suffix('.sty'),
                resolved_path.with_suffix('.cls'),
                resolved_path.with_suffix('.pdf'),
                resolved_path.with_suffix('.png'),
                resolved_path.with_suffix('.jpg'),
                resolved_path.with_suffix('.jpeg'),
                resolved_path.with_suffix('.eps')
            ]
            
            found = False
            for path in possible_paths:
                if path.exists():
                    found = True
                    break
                    
            if not found:
                missing_deps.append(orig_path)
                
        return missing_deps
    
    def compile_latex(self, tex_file):
        """Attempt to compile a LaTeX file."""
        # Create a temporary directory for output files only
        with tempfile.TemporaryDirectory() as temp_dir:
            try:
                # Run pdflatex from the file's directory to preserve relative paths
                # but output to temp directory to avoid cluttering source
                result = subprocess.run(
                    ['pdflatex', '-interaction=nonstopmode', '-output-directory', temp_dir, str(tex_file.name)],
                    capture_output=True,
                    text=True,
                    timeout=60,  # 60 second timeout
                    cwd=str(tex_file.parent)  # Run from the file's directory
                )
                
                return result.returncode == 0, result.stdout, result.stderr
                
            except subprocess.TimeoutExpired:
                return False, "", "Compilation timeout (>60s)"
            except FileNotFoundError:
                return False, "", "pdflatex not found - please install LaTeX"
            except Exception as e:
                return False, "", f"Compilation error: {str(e)}"
    

    
    def check_file(self, tex_file):
        """Check a single .tex file."""
        print(f"📄 Checking: {tex_file.relative_to(self.base_path)}")
        
        # First check dependencies
        missing_deps = self.check_dependencies(tex_file)
        if missing_deps:
            failure_info = {
                'file': tex_file,
                'type': 'Missing Dependencies',
                'details': missing_deps,
                'stdout': '',
                'stderr': ''
            }
            self.failures.append(failure_info)
            print(f"   ❌ Missing dependencies: {', '.join(missing_deps)}")
            return
        
        # Skip files that are meant to be included, not compiled standalone
        if self.is_include_file(tex_file):
            self.skipped.append(tex_file)
            print(f"   ⏭️  Skipped (include file)")
            return
            
        # Try compilation
        success, stdout, stderr = self.compile_latex(tex_file)
        
        if success:
            self.successes.append(tex_file)
            print(f"   ✅ Compilation successful")
        else:
            failure_info = {
                'file': tex_file,
                'type': 'Compilation Error',
                'details': self.extract_error_details(stdout, stderr),
                'stdout': stdout,
                'stderr': stderr
            }
            self.failures.append(failure_info)
            print(f"   ❌ Compilation failed")
    
    def is_include_file(self, tex_file):
        """Check if this is a file meant to be included, not compiled standalone."""
        # Files in ex_tex are typically includes - skip all of them
        if 'ex_tex' in str(tex_file):
            return True
            
        # Skip any file that contains '_sol_' or '_hw_' or '_ic_' in ex_tex context
        if any(pattern in tex_file.name for pattern in ['_sol_', '_hw_', '_ic_']) and 'ex_tex' in str(tex_file.parent):
            return True
            
        # Check if file has \documentclass - if not, it's likely an include
        try:
            with open(tex_file, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                if '\\documentclass' not in content and '\\begin{document}' not in content:
                    return True
        except Exception:
            pass
            
        return False
    
    def extract_error_details(self, stdout, stderr):
        """Extract meaningful error information from LaTeX output."""
        errors = []
        
        if stderr:
            errors.append(f"stderr: {stderr}")
            
        # Look for common LaTeX errors in stdout
        if stdout:
            lines = stdout.split('\n')
            for i, line in enumerate(lines):
                if any(error_type in line.lower() for error_type in 
                       ['error', 'undefined', 'missing', 'not found', 'failed']):
                    # Include some context around the error
                    start = max(0, i-2)
                    end = min(len(lines), i+3)
                    context = '\n'.join(lines[start:end])
                    errors.append(context)
                    
        return errors if errors else ["Unknown compilation error"]
    
    def generate_report(self):
        """Generate a comprehensive report of all findings."""
        report = []
        report.append("=" * 80)
        report.append("LATEX COMPILATION REPORT")
        report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("=" * 80)
        report.append("")
        
        total_files = len(self.successes) + len(self.failures) + len(self.skipped)
        report.append(f"📊 SUMMARY:")
        report.append(f"   Total files checked: {total_files}")
        report.append(f"   ✅ Successful compilations: {len(self.successes)}")
        report.append(f"   ❌ Failed compilations: {len(self.failures)}")
        report.append(f"   ⏭️  Skipped files: {len(self.skipped)}")
        report.append("")
        
        if self.failures:
            report.append("❌ COMPILATION FAILURES:")
            report.append("-" * 40)
            for i, failure in enumerate(self.failures, 1):
                report.append(f"\n{i}. {failure['file'].relative_to(self.base_path)}")
                report.append(f"   Type: {failure['type']}")
                if isinstance(failure['details'], list):
                    for detail in failure['details']:
                        report.append(f"   • {detail}")
                else:
                    report.append(f"   Details: {failure['details']}")
                report.append("")
        
        if self.successes:
            report.append("✅ SUCCESSFUL COMPILATIONS:")
            report.append("-" * 40)
            for success in self.successes:
                report.append(f"   ✅ {success.relative_to(self.base_path)}")
            report.append("")
            
        if self.skipped:
            report.append("⏭️  SKIPPED FILES (Include files):")
            report.append("-" * 40)
            for skipped in self.skipped:
                report.append(f"   ⏭️  {skipped.relative_to(self.base_path)}")
            report.append("")
        
        return '\n'.join(report)
    
    def run_check(self):
        """Run the complete check process."""
        print("🔍 Starting LaTeX compilation check...")
        print(f"📁 Base directory: {self.base_path}")
        print("📁 Checking directories: exercises/ and slides/")
        print("")
        
        tex_files = self.find_tex_files()
        print(f"📄 Found {len(tex_files)} .tex files")
        print("")
        
        for tex_file in tex_files:
            self.check_file(tex_file)
            
        print("\n" + "=" * 80)
        print(self.generate_report())
        
        # Save report to file
        report_file = self.base_path / "latex_compilation_report.txt"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(self.generate_report())
        print(f"\n📄 Full report saved to: {report_file}")
        
        return len(self.failures) == 0

def main():
    if len(sys.argv) > 1:
        base_path = sys.argv[1]
    else:
        base_path = os.getcwd()
    
    checker = LatexChecker(base_path)
    success = checker.run_check()
    
    if success:
        print("\n🎉 All LaTeX files compiled successfully!")
        sys.exit(0)
    else:
        print(f"\n⚠️  {len(checker.failures)} files failed to compile. See report for details.")
        sys.exit(1)

if __name__ == "__main__":
    main()