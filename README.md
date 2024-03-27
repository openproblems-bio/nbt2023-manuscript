# NBT2023 manuscript

Clone the repository

```bash
git clone https://github.com/openproblems-bio/nbt2023-manuscript.git
```

Change directory

```bash
cd nbt2023-manuscript
```

Check out submodules

```bash
git submodule update --init
```

## Install dependencies

```bash
Rscript -e 'install.packages(c("tidyverse", "funkyheatmap", "ggbeeswarm", "patchwork", "cowplot", "ggbreak", "ggrepel", "forcats"))'
```

## Generate figures

```bash
Rscript -e 'source("scripts/figure1a.R")'
Rscript -e 'source("scripts/figure2c.R")'
Rscript -e 'source("scripts/supnote2_figures.R")'
```

## Convert pdfs into pngs

```bash
scripts/convert_figures.sh
```