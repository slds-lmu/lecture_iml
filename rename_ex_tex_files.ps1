# Complete script to rename all ex_tex files and update all content references

$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# Define mappings for ex_tex file renamings
$exTexMappings = @{
    "04_feature_effects" = @{
        # hw files (originally numbered as 3 in ex_tex)
        "hw_3_1_PDP_ICE_Interactions.tex" = "hw_fe_1_PDP_ICE_Interactions.tex"
        "hw_3_2_ALE.tex" = "hw_fe_2_ALE.tex"
        "hw_3_3_Categorical_ALE.tex" = "hw_fe_3_Categorical_ALE.tex"
        "hw_3_4_ME.tex" = "hw_fe_4_ME.tex"
        "hw_sol_3_1_PDP_ICE_Interactions.tex" = "hw_sol_fe_1_PDP_ICE_Interactions.tex"
        "hw_sol_3_1_PDP_ICE_Interactions.R" = "hw_sol_fe_1_PDP_ICE_Interactions.R"
        "hw_sol_3_2_ALE.tex" = "hw_sol_fe_2_ALE.tex"
        "hw_sol_3_3_Categorical_ALE.tex" = "hw_sol_fe_3_Categorical_ALE.tex"
        "hw_sol_3_4_ME.tex" = "hw_sol_fe_4_ME.tex"
        "hw_sol_3_4_ME.R" = "hw_sol_fe_4_ME.R"
        # ic files (originally numbered as 4 in ex_tex)
        "ic_4_1_multiple_choice.tex" = "ic_fe_1_multiple_choice.tex"
        "ic_4_2_PDP_ALE_Plot.tex" = "ic_fe_2_PDP_ALE_Plot.tex"
        "ic_sol_4_1_multiple_choice.tex" = "ic_sol_fe_1_multiple_choice.tex"
        "ic_sol_4_2_PDP_ALE_Plot.tex" = "ic_sol_fe_2_PDP_ALE_Plot.tex"
        "ic_X_compute_mplot_pdp_ale.tex" = "ic_fe_X_compute_mplot_pdp_ale.tex"
        "ic_X_fe_trees.tex" = "ic_fe_X_fe_trees.tex"
        "ic_sol_X_compute_mplot_pdp_ale.tex" = "ic_sol_fe_X_compute_mplot_pdp_ale.tex"
        "ic_sol_X_fe_trees.tex" = "ic_sol_fe_X_fe_trees.tex"
    }
    "06_shapley" = @{
        # hw files (originally numbered as 4 in ex_tex)  
        "hw_4_1_Shapley_Value.tex" = "hw_shapley_1_Shapley_Value.tex"
        "hw_4_2_SHAP.tex" = "hw_shapley_2_SHAP.tex"
        "hw_4_3_LIME.tex" = "hw_shapley_3_LIME.tex"
        "hw_4_4_axioms_proofs.tex" = "hw_shapley_4_axioms_proofs.tex"
        "hw_sol_4_1_Shapley_Value.tex" = "hw_sol_shapley_1_Shapley_Value.tex"
        "hw_sol_4_2_SHAP.tex" = "hw_sol_shapley_2_SHAP.tex"
        "hw_sol_4_3_LIME.tex" = "hw_sol_shapley_3_LIME.tex"
        "hw_sol_4_4_axioms_proofs.tex" = "hw_sol_shapley_4_axioms_proofs.tex"
        # ic files (originally numbered as 6 in ex_tex)
        "ic_6_1_Quiz.tex" = "ic_shapley_1_Quiz.tex"
        "ic_6_2_Quiz.tex" = "ic_shapley_2_Quiz.tex"
        "ic_6_3_Improve_Explanation.tex" = "ic_shapley_3_Improve_Explanation.tex"
        "ic_6_4_Discussion.tex" = "ic_shapley_4_Discussion.tex"
        "ic_sol_6_1_Quiz.tex" = "ic_sol_shapley_1_Quiz.tex"
        "ic_sol_6_2_Quiz.tex" = "ic_sol_shapley_2_Quiz.tex"
        "ic_sol_6_3_Improve_Explanation.tex" = "ic_sol_shapley_3_Improve_Explanation.tex"
        "ic_sol_6_4_Discussion.tex" = "ic_sol_shapley_4_Discussion.tex"
    }
    "09_feature_importance_1" = @{
        # hw files (originally numbered as 5 in ex_tex)
        "hw_5_1_PFI.tex" = "hw_fi_1_PFI.tex"
        "hw_5_2_Conditional_Samping.tex" = "hw_fi_2_Conditional_Samping.tex"
        "hw_5_3_LOCO.tex" = "hw_fi_3_LOCO.tex"
        "hw_sol_5_1_PFI.tex" = "hw_sol_fi_1_PFI.tex"
        # Note: there's a duplicate hw_sol_4_1_Shapley_Value.tex that should be removed or renamed
        "hw_sol_4_1_Shapley_Value.tex" = "hw_sol_fi_1_Shapley_Value_DUPLICATE.tex"
        # ic files (originally numbered as 5 in ex_tex)
        "ic_5_1_Factorizing_Distributions.tex" = "ic_fi_1_Factorizing_Distributions.tex"
        "ic_5_2_FI_Extrapolation.tex" = "ic_fi_2_FI_Extrapolation.tex"
        "ic_5_3_Discussion.tex" = "ic_fi_3_Discussion.tex"
    }
}

# Function to rename files in ex_tex folders
function Rename-ExTexFiles {
    param(
        [string]$FolderName,
        [hashtable]$FileMap
    )
    
    $exTexPath = Join-Path (Join-Path $exercisesPath $FolderName) "ex_tex"
    
    if (Test-Path $exTexPath) {
        Write-Host "Renaming ex_tex files in: $FolderName" -ForegroundColor Green
        Push-Location $exTexPath
        
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

# Function to update main file contents with new ex_tex filenames
function Update-MainFileReferences {
    param(
        [string]$FolderName,
        [hashtable]$FileMap
    )
    
    $folderPath = Join-Path $exercisesPath $FolderName
    
    if (Test-Path $folderPath) {
        Write-Host "Updating main file references in: $FolderName" -ForegroundColor Green
        Push-Location $folderPath
        
        # Update all .tex files in the main folder
        Get-ChildItem -Filter "*.tex" | ForEach-Object {
            $filePath = $_.FullName
            $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
            
            if ($content) {
                $originalContent = $content
                
                # Replace each old filename with new filename in \input statements
                foreach ($oldName in $FileMap.Keys) {
                    $newName = $FileMap[$oldName]
                    $oldPath = "ex_tex/$oldName"
                    $newPath = "ex_tex/$newName"
                    
                    $content = $content -replace [regex]::Escape($oldPath), $newPath
                }
                
                if ($content -ne $originalContent) {
                    Write-Host "  Updating references in: $($_.Name)" -ForegroundColor Yellow
                    Set-Content $filePath $content -NoNewline
                }
            }
        }
        
        Pop-Location
    }
}

# Apply the renamings and updates
foreach ($folderName in $exTexMappings.Keys) {
    $fileMap = $exTexMappings[$folderName]
    
    # Rename the ex_tex files
    Rename-ExTexFiles -FolderName $folderName -FileMap $fileMap
    
    # Update references in main files
    Update-MainFileReferences -FolderName $folderName -FileMap $fileMap
}

Write-Host "`nEx_tex file renaming and reference updates completed!" -ForegroundColor Green