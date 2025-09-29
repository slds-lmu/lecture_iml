# Script to fix remaining file renamings that were missed

$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# Manual fixes for specific files that weren't renamed correctly
$manualFixes = @{
    "04_feature_effects" = @{
        "hw_4_fe.tex" = "hw_fe.tex"
        "hw_4_fe_2.tex" = "hw_fe_2.tex"
        "hw_4_fe_2_sol.tex" = "hw_fe_2_sol.tex" 
        "hw_4_fe_sol.tex" = "hw_fe_sol.tex"
        "hw_4_TO_DO_and_ideas.txt" = "hw_fe_TO_DO_and_ideas.txt"
        "ic_4_fe.tex" = "ic_fe.tex"
        "ic_4_fe_sol.tex" = "ic_fe_sol.tex"
        "ic_4_TO_DO_and_ideas.txt" = "ic_fe_TO_DO_and_ideas.txt"
    }
    "02_interpretable_models_1" = @{
        "hw_2_im.tex" = "hw_im.tex"
        "hw_2_im_sol.tex" = "hw_im_sol.tex"
        "hw_2_TO_DO_and_ideas.txt" = "hw_im_TO_DO_and_ideas.txt"
        "ic_2_im.tex" = "ic_im.tex"
        "ic_2_im_sol.tex" = "ic_im_sol.tex"
        "ic_2_TO_DO_and_ideas.txt" = "ic_im_TO_DO_and_ideas.txt"
    }
    "03_interpretable_models_2" = @{
        "hw_3_im_2.tex" = "hw_im2.tex"
        "hw_3_im_2_sol.tex" = "hw_im2_sol.tex"
        "hw_3_TO_DO_and_ideas.txt" = "hw_im2_TO_DO_and_ideas.txt"
        "ic_3_im_2.tex" = "ic_im2.tex"
        "ic_3_im_2_sol.tex" = "ic_im2_sol.tex"
        "ic_3_TO_DO_and_ideas.txt" = "ic_im2_TO_DO_and_ideas.txt"
    }
    "05_functional-decompositions" = @{
        "hw_5_func_decomp.tex" = "hw_func_decomp.tex"
        "hw_5_func_decomp_sol.tex" = "hw_func_decomp_sol.tex"
        "hw_5_TO_DO_and_ideas.txt" = "hw_func_decomp_TO_DO_and_ideas.txt"
        "ic_5_func_decomp.tex" = "ic_func_decomp.tex"
        "ic_5_func_decomp_sol.tex" = "ic_func_decomp_sol.tex"
        "ic_5_2_func_decomp.tex" = "ic_func_decomp_2.tex"
        "ic_5_2_func_decomp_sol.tex" = "ic_func_decomp_2_sol.tex"
        "ic_5_TO_DO_and_ideas.txt" = "ic_func_decomp_TO_DO_and_ideas.txt"
    }
    "09_feature_importance_1" = @{
        "hw_5_TO_DO_and_ideas.txt" = "hw_fi_TO_DO_and_ideas.txt"
        "hw_sheet_5_fi.tex" = "hw_fi.tex"
        "hw_sol_sheet_5_fi.tex" = "hw_fi_sol.tex"
        "ic_5_TO_DO_and_ideas.txt" = "ic_fi_TO_DO_and_ideas.txt"
        "ic_sheet_5_fi.tex" = "ic_fi.tex"
    }
    "11_local_lime" = @{
        "hw_6_le_1.tex" = "hw_lime_1.tex"
        "hw_6_le_1_sol.tex" = "hw_lime_1_sol.tex"
        "hw_6_le_2.tex" = "hw_lime_2.tex"
        "hw_6_le_2_sol.tex" = "hw_lime_2_sol.tex"
        "hw_6_TO_DO_and_ideas.txt" = "hw_lime_TO_DO_and_ideas.txt"
        "ic_6_le_1.tex" = "ic_lime_1.tex"
        "ic_6_le_1_sol.tex" = "ic_lime_1_sol.tex"
        "ic_6_le_2.tex" = "ic_lime_2.tex"
        "ic_6_le_2_sol.tex" = "ic_lime_2_sol.tex"
        "ic_6_TO_DO_and_ideas.txt" = "ic_lime_TO_DO_and_ideas.txt"
    }
}

# Function to process manual fixes
function Apply-ManualFixes {
    param(
        [string]$FolderName,
        [hashtable]$FileMap
    )
    
    $folderPath = Join-Path $exercisesPath $FolderName
    
    if (Test-Path $folderPath) {
        Write-Host "Processing manual fixes for: $FolderName" -ForegroundColor Green
        Push-Location $folderPath
        
        foreach ($oldName in $FileMap.Keys) {
            $newName = $FileMap[$oldName]
            
            if (Test-Path $oldName) {
                Write-Host "  Renaming: $oldName -> $newName" -ForegroundColor Yellow
                Rename-Item $oldName $newName -ErrorAction SilentlyContinue
            }
        }
        
        Pop-Location
    }
}

# Apply manual fixes
foreach ($folder in $manualFixes.Keys) {
    Apply-ManualFixes -FolderName $folder -FileMap $manualFixes[$folder]
}

Write-Host "`nManual fixes completed!" -ForegroundColor Green