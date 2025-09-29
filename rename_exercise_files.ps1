# PowerShell script to rename exercise files from numbered to topic-based naming
# This script removes numbers from exercise file names and uses topic names instead

$exercisesPath = "c:\Users\hayk_\OneDrive\Desktop\lecture_iml\exercises"

# Define the mapping from numbered folders to topic names with their numbers
$folderMappings = @{
    "01_introduction" = @{ topic = "intro"; numbers = @("1", "01") }
    "02_interpretable_models_1" = @{ topic = "im"; numbers = @("2", "02") }
    "03_interpretable_models_2" = @{ topic = "im2"; numbers = @("3", "03") } 
    "04_feature_effects" = @{ topic = "fe"; numbers = @("3", "4", "03", "04") }
    "05_functional-decompositions" = @{ topic = "func_decomp"; numbers = @("5", "05") }
    "06_shapley" = @{ topic = "shapley"; numbers = @("4", "6", "04", "06") }
    "08_regional_effects" = @{ topic = "regional"; numbers = @("8", "08") }
    "09_feature_importance_1" = @{ topic = "fi"; numbers = @("5", "9", "05", "09") }
    "10_feature_importance_2" = @{ topic = "fi2"; numbers = @("10") }
    "11_local_lime" = @{ topic = "lime"; numbers = @("6", "11") }
    "12_local_counterfactual" = @{ topic = "counterfactual"; numbers = @("12") }
}

# Function to rename files in a directory
function Rename-ExerciseFiles {
    param(
        [string]$Directory,
        [string]$TopicName,
        [string]$OriginalNumber
    )
    
    Write-Host "Processing directory: $Directory" -ForegroundColor Green
    
    if (Test-Path $Directory) {
        Push-Location $Directory
        
        # Rename main exercise files
        $mainFiles = @(
            @{ Pattern = "hw_${OriginalNumber}_*"; Replacement = "hw_${TopicName}" }
            @{ Pattern = "ic_${OriginalNumber}_*"; Replacement = "ic_${TopicName}" }
            @{ Pattern = "hw_sol_sheet_${OriginalNumber}_*"; Replacement = "hw_sol_${TopicName}" }
            @{ Pattern = "hw_sheet_${OriginalNumber}_*"; Replacement = "hw_${TopicName}" }
            @{ Pattern = "ic_sheet_${OriginalNumber}_*"; Replacement = "ic_${TopicName}" }
        )
        
        Get-ChildItem -File | ForEach-Object {
            $oldName = $_.Name
            $newName = $oldName
            
            # Apply main file renamings
            foreach ($mapping in $mainFiles) {
                if ($oldName -like $mapping.Pattern) {
                    $newName = $oldName -replace "^(hw|ic)(_sol)?(_sheet)?_${OriginalNumber}_", "`$1`$2_${TopicName}_"
                    break
                }
            }
            
            # Handle TO_DO files specially
            if ($oldName -like "*_${OriginalNumber}_TO_DO_and_ideas.txt") {
                $newName = $oldName -replace "_${OriginalNumber}_TO_DO_and_ideas", "_${TopicName}_TO_DO_and_ideas"
            }
            
            # Rename if different
            if ($newName -ne $oldName) {
                Write-Host "  Renaming: $oldName -> $newName" -ForegroundColor Yellow
                Rename-Item $oldName $newName -ErrorAction SilentlyContinue
            }
        }
        
        # Process ex_tex subdirectory
        if (Test-Path "ex_tex") {
            Write-Host "  Processing ex_tex subdirectory..." -ForegroundColor Cyan
            Push-Location "ex_tex"
            
            Get-ChildItem -File | ForEach-Object {
                $oldName = $_.Name
                $newName = $oldName
                
                # Replace hw_X_Y_ with hw_topic_Y_
                if ($oldName -match "^(hw|ic)(_sol)?_${OriginalNumber}_(\d+)_(.+)") {
                    $prefix = $matches[1]
                    $sol = $matches[2]
                    $exerciseNum = $matches[3]
                    $rest = $matches[4]
                    $newName = "${prefix}${sol}_${TopicName}_${exerciseNum}_${rest}"
                }
                # Handle cases without exercise numbers
                elseif ($oldName -match "^(hw|ic)(_sol)?_${OriginalNumber}_(.+)") {
                    $prefix = $matches[1]
                    $sol = $matches[2]
                    $rest = $matches[3]
                    $newName = "${prefix}${sol}_${TopicName}_${rest}"
                }
                
                if ($newName -ne $oldName) {
                    Write-Host "    Renaming: $oldName -> $newName" -ForegroundColor Yellow
                    Rename-Item $oldName $newName -ErrorAction SilentlyContinue
                }
            }
            
            Pop-Location
        }
        
        # Process rsrc subdirectory
        if (Test-Path "rsrc") {
            Write-Host "  Processing rsrc subdirectory..." -ForegroundColor Cyan
            Push-Location "rsrc"
            
            Get-ChildItem -File | ForEach-Object {
                $oldName = $_.Name
                $newName = $oldName
                
                # Replace references to sheet numbers
                if ($oldName -match "sheet_${OriginalNumber}_") {
                    $newName = $oldName -replace "sheet_${OriginalNumber}_", "sheet_${TopicName}_"
                }
                
                if ($newName -ne $oldName) {
                    Write-Host "    Renaming: $oldName -> $newName" -ForegroundColor Yellow
                    Rename-Item $oldName $newName -ErrorAction SilentlyContinue
                }
            }
            
            Pop-Location
        }
        
        Pop-Location
    }
}

# Function to update file contents with new paths
function Update-FileContents {
    param(
        [string]$Directory,
        [string]$TopicName,
        [string]$OriginalNumber
    )
    
    Write-Host "Updating file contents in: $Directory" -ForegroundColor Green
    
    if (Test-Path $Directory) {
        Push-Location $Directory
        
        # Update main .tex files
        Get-ChildItem -Filter "*.tex" | ForEach-Object {
            $filePath = $_.FullName
            $content = Get-Content $filePath -Raw -ErrorAction SilentlyContinue
            
            if ($content) {
                $originalContent = $content
                
                # Update input statements in main files
                $content = $content -replace "\\input\{ex_tex/hw_${OriginalNumber}_(\d+)_", "\input{ex_tex/hw_${TopicName}_`$1_"
                $content = $content -replace "\\input\{ex_tex/hw_sol_${OriginalNumber}_(\d+)_", "\input{ex_tex/hw_sol_${TopicName}_`$1_"
                $content = $content -replace "\\input\{ex_tex/ic_${OriginalNumber}_(\d+)_", "\input{ex_tex/ic_${TopicName}_`$1_"
                $content = $content -replace "\\input\{ex_tex/ic_sol_${OriginalNumber}_(\d+)_", "\input{ex_tex/ic_sol_${TopicName}_`$1_"
                
                # Handle cases without numbers
                $content = $content -replace "\\input\{ex_tex/hw_${OriginalNumber}_", "\input{ex_tex/hw_${TopicName}_"
                $content = $content -replace "\\input\{ex_tex/hw_sol_${OriginalNumber}_", "\input{ex_tex/hw_sol_${TopicName}_"
                $content = $content -replace "\\input\{ex_tex/ic_${OriginalNumber}_", "\input{ex_tex/ic_${TopicName}_"
                $content = $content -replace "\\input\{ex_tex/ic_sol_${OriginalNumber}_", "\input{ex_tex/ic_sol_${TopicName}_"
                
                if ($content -ne $originalContent) {
                    Write-Host "  Updating contents of: $($_.Name)" -ForegroundColor Yellow
                    Set-Content $filePath $content -NoNewline
                }
            }
        }
        
        Pop-Location
    }
}

# Main execution
Write-Host "Starting exercise file renaming process..." -ForegroundColor Magenta
Write-Host "Base path: $exercisesPath" -ForegroundColor Magenta

foreach ($folder in $folderMappings.Keys) {
    $fullPath = Join-Path $exercisesPath $folder
    $topicName = $folderMappings[$folder]
    
    # Extract the original number from folder name
    if ($folder -match "^(\d+)_") {
        $originalNumber = $matches[1]
        
        Write-Host "`nProcessing folder: $folder (number: $originalNumber, topic: $topicName)" -ForegroundColor Magenta
        
        # Rename files
        Rename-ExerciseFiles -Directory $fullPath -TopicName $topicName -OriginalNumber $originalNumber
        
        # Update file contents
        Update-FileContents -Directory $fullPath -TopicName $topicName -OriginalNumber $originalNumber
    }
}

Write-Host "`nExercise file renaming completed!" -ForegroundColor Green
Write-Host "Please review the changes and test the LaTeX compilation." -ForegroundColor Yellow