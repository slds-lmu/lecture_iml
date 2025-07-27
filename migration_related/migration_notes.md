**DISCLAIMER:** This document is a work in progress. It's not well structured and sometimes may be outdated. I'm not deleting it, just in case it may be useful for someone.

# Links
Migration:
1. [Temporary OverLeaf](https://www.overleaf.com/read/kcdyhfgwybvw#2ca12f)
2. [Temporary repo](https://github.com/HaykTarkhanyan/lecture_service_attempt)
3. [Google Sheet for status tracking](https://docs.google.com/spreadsheets/d/1CUaNZJt9qlAQZcsy_i1eFSv5n2B-eL_tfbIdLOAQUAM/edit?usp=sharing)
4. [Rendered PDFs (old and new)](https://github.com/HaykTarkhanyan/lecture_service_attempt/tree/main/pdfs)

Useful links:
1. [Lecture Service Repo](https://github.com/slds-lmu/lecture_service)
2. [Lecture Service Wiki](https://github.com/slds-lmu/lecture_service/wiki/Slides)
3. [Lecture Debug](https://github.com/slds-lmu/lecture_debug)
4. [IML Repo](https://github.com/slds-lmu/lecture_iml/tree/master)
5. [SL Repo (for reference)](https://github.com/slds-lmu/lecture_sl/)
  


# Importing `lecture_service` files into your repo
If I'm not mistaken [`lecture_service`](https://github.com/slds-lmu/lecture_service) assumes Linux. At least we need to run a bash file, so if you're using Windows, the easiest way I found was going to Microsoft Store and installing WSL (Windows Subsystem for Linux) with Ubuntu, [download link](https://apps.microsoft.com/detail/9PDXGNCFSCZV?hl=en-us&gl=AM&ocid=pdpshare).

Open the Ubuntu terminal
```bash
git clone https://github.com/slds-lmu/lecture_iml # or another repo
cd lecture_iml
```

After that inside the repo in which you want to integrate the service place the following file - [`update-service.sh`](https://github.com/slds-lmu/lecture_service/blob/main/service/scripts/update-service.sh) 

Then run
```bash
bash update-service.sh
```
After that a repo called `scripts` which contains the `update-service.sh` file will be created. You can delete the manually inserted file now.

Also, you will see a number of new files, especially in the `style` folder.

# (Optional) Overleaf (OL) integration
If you're working in a separate repo you will most likely want to sync it with OL. You are gonna need a paid version of OL and also you may get an error stating that maximum number of files (2000) has been exceeded. You can run `git ls-files | wc -l` to get the count of the files in a given directory. If you want to get distribution of file counts per directory you can run `find . -maxdepth 1 -mindepth 1 -type d | while read dir; do   printf "%-25.25s : " "$dir";   find "$dir" -type f | wc -l; done`.


# Steps
1. You can use the [migration_utils.ipynb](migration_utils.ipynb) notebook to help with the migration. It will do most of the boring work for you, and usually you will only need to fix the hanging words issue. Below are overall the changes that one needs to do:
2. First line should be `\documentclass[11pt,compress,t,notes=noshow, xcolor=table]{beamer}`
3. Comment out `\usepackage{../../style/lmu-lecture}`
4. Add `\input{../../style/preamble}`
5. Rename `blackbox_flashlight_left.pdf` from the `style` folder to `logo.pdf`
6. Change title slide in this way:
```latex
\newcommand{\titlefigure}{figure/performance_vs_interpretability}
\newcommand{\learninggoals}{
\item Why interpretability?
\item Developments until now?
\item Use cases for interpretability}


\lecturechapter{Introduction, Motivation, and History}
\lecture{Interpretable Machine Learning}
```
becomes
```latex
\titlemeta{
Interpretable Machine Learning % commenting out \title from line 10, since here we don't have heading-subheading structure (helps avoid duplication of the title)
}{
Introduction, Motivation, and History
}{
figure/performance_vs_interpretability
}{
\item Why interpretability?
\item Developments until now?
\item Use cases for interpretability
}
```
6. Change `\begin{columns}` to `\splitVCC` or `\splitVTT` [more info](https://github.com/slds-lmu/lecture_service/wiki/Slides#splitv-column-layout-helpers)
```latex
\splitVCC[0.8]{ % 0.8 is the width of the first column
  \begin{itemize}
    \item Example itemize content for centered columns
    \item Second itemize item
  \end{itemize}
  }{
  Lorem ipsum dolor sit amet, consectetur adipiscing elit
  }
```
7. Note: when using `\splitVCC` and having content splited into two frames, for some reason the second column moves when the slide changes
8. For URL's (not papers) change `\citebutton{Some Text}{https:something}` to `\sourceref{https:something}`. The problem is that the text is hardcoded to `Click for source`. Don't know if this is problematic. Maybe a workaround is to pretend it's a paper and specify the text as an author. UPDATE: currently we use that workariund
9. When citing papers we need to create a `references.bib` file for each chunk folder. 
```bibtex
\citebutton{Goodman \& Flaxman (2017)}{https://doi.org/10.1609/aimag.v38i3.2741}
``` 
get's replaced with 
```latex
references.bib file
@article{Goodman_Flaxman,
  author={Goodman and Flaxman},
  year={2017},
  url={https://doi.org/10.1609/aimag.v38i3.2741}
}

and actual code 
\furtherreading{Goodman_Flaxman}
```
Further notes:
Sometimes the label will get formatted weirdly, e.g. sometimes the CITEKEY will be displayed, when we don't have a year it will display as "n. d." and sometimes "et. al"-s will get lost. 
Partial solution is to not provide the `author` in the bibtex file, but instead provide a `title`, downside is that the title will be put in quotes. But this way we have full control over the text that is displayed. But the "n. d." will still be there :-(

Note that when the reference is in the frame title it will get displayed as CITEKEY all uppercase
10.   If you want to use `\color` you'll need to add `\usepackage{xcolor}`, it no longer comes with `common.tex`. You may also need to add
```latex
\definecolor{ggred}{rgb}{0.973, 0.463, 0.427}
\definecolor{ggblue}{rgb}{0, 0.749, 0.769}
\definecolor{fgcolor}{rgb}{0.345, 0.345, 0.345}
```
12.  You may sometimes need to import staff from `latex-math` because it has been moved from `common.tex`. Usually you need `basic-math.tex`, `basic-ml.tex` and sometimes `ml-interpretable.tex`

