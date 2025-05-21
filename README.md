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

      - name: Checkout gh-pages
        uses: actions/checkout@v4
        with:
          ref: gh-pages
          token: ${{ secrets.ARK_CICD_SERVICE_ACCOUNT }}

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

args <- commandArgs(trailingOnly = TRUE)

# generate site content
main(args[1])

# END
```

