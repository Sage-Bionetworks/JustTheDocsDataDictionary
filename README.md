## Example GitHub Actions workflow: ci-gh-pages.yml

To setup R environment and run script to build site content. NOTE - this example 
does not include workflow steps to checkout your repo or commit changes.

```yaml
name: ci gh-pages content update

on:
  push:
    paths:
      - '.github/workflows/ci-gh-pages.yml'
      - '_utils/update_site_content.R'
      - 'model.csv'
  # Allows you to run this workflow manually from the Actions tab
  workflow_dispatch:

jobs:
  update:
    name: Update site content
    runs-on: ubuntu-latest
    env:
      GITHUB_TOKEN: ${{ github.token }}

    steps:
      - name: Setup R env
        uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true
          r-version: '4.3.3'

      - name: Install packages
        uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::dplyr
            any::stringr
            any::glue
            any::purrr
            any::snakecase
            any::rlang
            any::remotes
            any::lubridate
            any::openxlsx

      - name: Checkout gh-pages
        uses: actions/checkout@v4
        with:
          ref: main
          token: ${{ github.token }}

      - name: Checkout model files write to subdir
        uses: actions/checkout@v4
        with:
          repository: ${{ github.repository }} # Checkout the same repository
          ref: main # Specify the branch you want to access
          path: main # Checkout into a different directory
          token: ${{ github.token }}

      - name: Clean up before commit
        run: |
          rm -Rf main/

      - name: Update site content
        run: |
          Rscript _utils/update_site_content.R <https://raw.githubusercontent.com/ URL to your model.csv>
```

## Example: update_site_content.R

```r
#! Rscript

# install remote package
remotes::install_github("Sage-Bionetworks/JustTheDocsDataDictionary")
# load installed library
library(JustTheDocsDataDictionary)

# generate site content
main(portal = <portal>, branch = "main")

# END
```

For more details on running `main`, see documentation.

