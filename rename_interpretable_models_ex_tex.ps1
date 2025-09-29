# Script to rename ex_tex files in interpretable models folders

$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# Define mappings for interpretable models ex_tex files
$interpretableModelsMapping = @{
    "02_interpretable_models_1" = @{
        # hw files
        "hw_2_1_gam.tex" = "hw_im_1_gam.tex"
        "hw_2_2_standard_error.tex" = "hw_im_2_standard_error.tex"
        "hw_2_3_glm.tex" = "hw_im_3_glm.tex"
        "hw_2_4_trees_optimization.tex" = "hw_im_4_trees_optimization.tex"
        "hw_sol_2_1_gam.tex" = "hw_sol_im_1_gam.tex"
        "hw_sol_2_2_standard_error.tex" = "hw_sol_im_2_standard_error.tex"
        "hw_sol_2_3_glm.tex" = "hw_sol_im_3_glm.tex"
        "hw_sol_2_4_trees_optimization.tex" = "hw_sol_im_4_trees_optimization.tex"
        # ic files
        "ic_2_1_true_false.tex" = "ic_im_1_true_false.tex"
        "ic_sol_2_1_true_false.tex" = "ic_sol_im_1_true_false.tex"
    }
    "03_interpretable_models_2" = @{
        # hw files
        "hw_3_1_feature_importance.tex" = "hw_im2_1_feature_importance.tex"
        "hw_3_2_gam_training.tex" = "hw_im2_2_gam_training.tex"
        "hw_3_3_ebm_training.tex" = "hw_im2_3_ebm_training.tex"
        "hw_sol_3_1_feature_importance.tex" = "hw_sol_im2_1_feature_importance.tex"
        "hw_sol_3_2_gam_training.tex" = "hw_sol_im2_2_gam_training.tex"
        "hw_sol_3_3_ebm_training.tex" = "hw_sol_im2_3_ebm_training.tex"
        # ic files
        "ic_3_1_true_false.tex" = "ic_im2_1_true_false.tex"
        "ic_sol_3_1_true_false.tex" = "ic_sol_im2_1_true_false.tex"
    }
}

# Function to rename files in ex_tex folders
function Rename-InterpretableModelsExTexFiles {
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
function Update-InterpretableModelsReferences {
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
                    
                    # Handle both with and without .tex extension
                    $oldPathNoExt = $oldPath -replace '\.tex$', ''
                    $newPathNoExt = $newPath -replace '\.tex$', ''
                    
                    $content = $content -replace [regex]::Escape($oldPath), $newPath
                    $content = $content -replace [regex]::Escape($oldPathNoExt), $newPathNoExt
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

# Apply the renamings and updates for interpretable models folders
foreach ($folderName in $interpretableModelsMapping.Keys) {
    $fileMap = $interpretableModelsMapping[$folderName]
    
    # Rename the ex_tex files
    Rename-InterpretableModelsExTexFiles -FolderName $folderName -FileMap $fileMap
    
    # Update references in main files
    Update-InterpretableModelsReferences -FolderName $folderName -FileMap $fileMap
}

Write-Host "`nInterpretable models ex_tex file renaming completed!" -ForegroundColor Green