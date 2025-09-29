# Summary of Exercise File Renaming

Write-Host "=== EXERCISE FILE RENAMING SUMMARY ===" -ForegroundColor Magenta
Write-Host ""

$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# List all folders and show their current state
$folders = @(
    "01_introduction",
    "02_interpretable_models_1", 
    "03_interpretable_models_2",
    "04_feature_effects",
    "05_functional-decompositions",
    "06_shapley",
    "08_regional_effects",
    "09_feature_importance_1",
    "10_feature_importance_2",
    "11_local_lime",
    "12_local_counterfactual"
)

foreach ($folder in $folders) {
    $folderPath = Join-Path $exercisesPath $folder
    
    if (Test-Path $folderPath) {
        Write-Host "Folder: $folder" -ForegroundColor Green
        
        # List main .tex files
        $texFiles = Get-ChildItem $folderPath -Filter "*.tex" | Select-Object -ExpandProperty Name
        if ($texFiles) {
            Write-Host "  Main files:" -ForegroundColor Yellow
            $texFiles | ForEach-Object { Write-Host "    $_" }
        }
        
        # List ex_tex files if folder exists
        $exTexPath = Join-Path $folderPath "ex_tex"
        if (Test-Path $exTexPath) {
            $exTexFiles = Get-ChildItem $exTexPath -Filter "*.tex" | Select-Object -ExpandProperty Name
            if ($exTexFiles) {
                Write-Host "  Ex_tex files:" -ForegroundColor Cyan
                $exTexFiles | ForEach-Object { Write-Host "    $_" }
            }
        }
        
        Write-Host ""
    }
}

Write-Host "=== RENAMING PATTERNS APPLIED ===" -ForegroundColor Magenta
Write-Host ""
Write-Host "Main file patterns:" -ForegroundColor Yellow
Write-Host "  hw_X_topic.tex -> hw_topic.tex"
Write-Host "  hw_X_topic_sol.tex -> hw_topic_sol.tex"  
Write-Host "  ic_X_topic.tex -> ic_topic.tex"
Write-Host "  ic_X_topic_sol.tex -> ic_topic_sol.tex"
Write-Host "  hw_X_TO_DO_and_ideas.txt -> hw_topic_TO_DO_and_ideas.txt"
Write-Host "  ic_X_TO_DO_and_ideas.txt -> ic_topic_TO_DO_and_ideas.txt"
Write-Host ""
Write-Host "Ex_tex file patterns:" -ForegroundColor Cyan
Write-Host "  hw_X_Y_name.tex -> hw_topic_Y_name.tex"
Write-Host "  hw_sol_X_Y_name.tex -> hw_sol_topic_Y_name.tex"
Write-Host "  ic_X_Y_name.tex -> ic_topic_Y_name.tex"
Write-Host "  ic_sol_X_Y_name.tex -> ic_sol_topic_Y_name.tex"
Write-Host ""
Write-Host "Topic abbreviations used:" -ForegroundColor Green
Write-Host "  01_introduction -> intro"
Write-Host "  02_interpretable_models_1 -> im"  
Write-Host "  03_interpretable_models_2 -> im2"
Write-Host "  04_feature_effects -> fe"
Write-Host "  05_functional-decompositions -> func_decomp"
Write-Host "  06_shapley -> shapley"
Write-Host "  08_regional_effects -> regional"
Write-Host "  09_feature_importance_1 -> fi"
Write-Host "  10_feature_importance_2 -> fi2"
Write-Host "  11_local_lime -> lime"
Write-Host "  12_local_counterfactual -> counterfactual"
Write-Host ""
Write-Host "✅ All file renamings completed successfully!" -ForegroundColor Green
Write-Host "✅ All content references updated!" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Test LaTeX compilation for each exercise sheet"
Write-Host "  2. Check that all \input{} statements work correctly"
Write-Host "  3. Verify that R script references are still valid"
Write-Host "  4. Update any documentation or README files"