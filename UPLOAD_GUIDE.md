# Guide: How to Upload Files to a GitHub Repository

This guide explains how to upload local files to this repository.

## Method 1: Using Git Command Line

### Step 1: Clone the Repository (if you haven't already)
```bash
git clone https://github.com/chrisleboa/extracct.git
cd extracct
```

### Step 2: Add Your File
Place your file in the repository directory. For example:
```bash
# Copy a file from your local system
cp /path/to/your/file.txt ./
```

### Step 3: Stage Your File
```bash
git add file.txt
# Or to add all files:
git add .
```

### Step 4: Commit Your Changes
```bash
git commit -m "Add file.txt"
```

### Step 5: Push to GitHub
```bash
git push origin main
```

## Method 2: Using GitHub Web Interface

1. Navigate to https://github.com/chrisleboa/extracct
2. Click on "Add file" button
3. Select "Upload files"
4. Drag and drop your files or click "choose your files"
5. Add a commit message
6. Click "Commit changes"

## Method 3: Using GitHub Desktop

1. Open GitHub Desktop
2. Clone the repository or open it if already cloned
3. Copy your files into the repository folder
4. GitHub Desktop will automatically detect the changes
5. Add a commit message in the summary field
6. Click "Commit to main"
7. Click "Push origin"

## Example: Adding a Data File

To add a CSV file with data:
```bash
# Navigate to your repository
cd extracct

# Copy your data file
cp ~/Downloads/my_data.csv ./data/

# Add and commit
git add data/my_data.csv
git commit -m "Add new dataset: my_data.csv"

# Push to GitHub
git push origin main
```

## Tips

- **Use descriptive commit messages**: Explain what the file contains and why it's being added
- **Organize files in folders**: Keep related files together (e.g., data files in a `data/` folder)
- **Check file size**: GitHub has a 100 MB file size limit. For larger files, consider using Git LFS
- **Don't commit sensitive data**: Never upload files containing passwords, API keys, or personal information
- **Update .gitignore**: Add patterns for files that shouldn't be tracked (e.g., temporary files, build artifacts)

## Common Issues

### Issue: File is too large
```
Solution: Use Git Large File Storage (Git LFS)
git lfs install
git lfs track "*.csv"
git add .gitattributes
git add your-large-file.csv
git commit -m "Add large file with LFS"
```

### Issue: Permission denied
```
Solution: Check your GitHub authentication
git config --global user.email "your-email@example.com"
git config --global user.name "Your Name"
```

### Issue: Merge conflicts
```
Solution: Pull the latest changes first
git pull origin main
# Resolve any conflicts
git add .
git commit -m "Resolve merge conflicts"
git push origin main
```
