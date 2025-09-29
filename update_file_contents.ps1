# Script to update file contents after renaming
$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# Define all the content updates needed
$contentUpdates = @{
    "01_introduction" = @{
        # Already done by previous script
    }
    "02_interpretable_models_1" = @{
        "hw_im.tex" = @{
            "\\input{ex_tex/hw_2_" = "\input{ex_tex/hw_im_"
        }
        "hw_im_sol.tex" = @{
            "\\input{ex_tex/hw_sol_2_" = "\input{ex_tex/hw_sol_im_"
        }
        "ic_im.tex" = @{
            "\\input{ex_tex/ic_2_" = "\input{ex_tex/ic_im_"
        }
        "ic_im_sol.tex" = @{
            "\\input{ex_tex/ic_sol_2_" = "\input{ex_tex/ic_sol_im_"
        }
    }
    "03_interpretable_models_2" = @{
        "hw_im2.tex" = @{
            "\\input{ex_tex/hw_3_" = "\input{ex_tex/hw_im2_"
        }
        "hw_im2_sol.tex" = @{
            "\\input{ex_tex/hw_sol_3_" = "\input{ex_tex/hw_sol_im2_"
        }
        "ic_im2.tex" = @{
            "\\input{ex_tex/ic_3_" = "\input{ex_tex/ic_im2_"
        }
        "ic_im2_sol.tex" = @{
            "\\input{ex_tex/ic_sol_3_" = "\input{ex_tex/ic_sol_im2_"
        }
    }
    "04_feature_effects" = @{
        "hw_fe.tex" = @{
            "\\input{ex_tex/hw_3_" = "\input{ex_tex/hw_fe_"
        }
        "hw_fe_sol.tex" = @{
            "\\input{ex_tex/hw_sol_3_" = "\input{ex_tex/hw_sol_fe_"
        }
        "ic_fe.tex" = @{
            "\\input{ex_tex/ic_4_" = "\input{ex_tex/ic_fe_"
        }
        "ic_fe_sol.tex" = @{
            "\\input{ex_tex/ic_sol_4_" = "\input{ex_tex/ic_sol_fe_"
        }
    }
    "05_functional-decompositions" = @{
        "hw_func_decomp.tex" = @{
            "\\input{ex_tex/hw_5_" = "\input{ex_tex/hw_func_decomp_"
        }
        "hw_func_decomp_sol.tex" = @{
            "\\input{ex_tex/hw_sol_5_" = "\input{ex_tex/hw_sol_func_decomp_"
        }
        "ic_func_decomp.tex" = @{
            "\\input{ex_tex/ic_5_" = "\input{ex_tex/ic_func_decomp_"
        }
        "ic_func_decomp_sol.tex" = @{
            "\\input{ex_tex/ic_sol_5_" = "\input{ex_tex/ic_sol_func_decomp_"
        }
    }
    "06_shapley" = @{
        # Files were already processed but may need content updates
    }
    "09_feature_importance_1" = @{
        "hw_fi.tex" = @{
            "\\input{ex_tex/hw_5_" = "\input{ex_tex/hw_fi_"
        }
        "hw_fi_sol.tex" = @{
            "\\input{ex_tex/hw_sol_5_" = "\input{ex_tex/hw_sol_fi_"
        }
        "ic_fi.tex" = @{
            "\\input{ex_tex/ic_5_" = "\input{ex_tex/ic_fi_"
        }
    }
    "11_local_lime" = @{
        # Complex file structures - will handle manually if needed
    }
}

function Update-FileContent {
    param(
        [string]$FolderPath,
        [string]$FileName,
        [hashtable]$Replacements
    )
    
    $filePath = Join-Path $FolderPath $FileName
    
    if (Test-Path $filePath) {
        $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
        
        if ($content) {
            $originalContent = $content
            
            foreach ($oldPattern in $Replacements.Keys) {
                $newPattern = $Replacements[$oldPattern]
                $content = $content -replace [regex]::Escape($oldPattern), $newPattern
            }
            
            if ($content -ne $originalContent) {
                Write-Host "  Updating content in: $FileName" -ForegroundColor Yellow
                Set-Content $filePath $content -NoNewline
            }
        }
    }
}

# Process each folder
foreach ($folderName in $contentUpdates.Keys) {
    $folderPath = Join-Path $exercisesPath $folderName
    
    if ((Test-Path $folderPath) -and ($contentUpdates[$folderName].Count -gt 0)) {
        Write-Host "Updating content in folder: $folderName" -ForegroundColor Green
        
        foreach ($fileName in $contentUpdates[$folderName].Keys) {
            $replacements = $contentUpdates[$folderName][$fileName]
            Update-FileContent -FolderPath $folderPath -FileName $fileName -Replacements $replacements
        }
    }
}

Write-Host "`nContent updates completed!" -ForegroundColor Green