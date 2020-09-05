![Banner](https://github.com/team-fsa-s2ds-alumni/foobar-dashboard/blob/master/static/banner.png)

**F**ood **O**f **O**nline **B**usinesses **A**nd **R**estaurants

<hr>

*An interactive tool to map the online food ecosystem.*

<br>

![CI (pip)](https://github.com/team-fsa-s2ds-alumni/foobar-dashboard/workflows/CI%20(conda)/badge.svg)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

`FOOBAR` provides an interactive dashboard for navigating the landscape of online food platforms, powered by a relational database.

## Documentation

The official documentation is hosted on this repository's [wiki](https://github.com/team-fsa-s2ds-alumni/foobar-dashboard/wiki).

## Install

```
git clone https://github.com/team-fsa-s2ds-alumni/foobar-dashboard.git
cd foobar-dashboard
conda env create -f environment.yml
conda activate foobar-dashboard
```

## Update conda environment with yaml
```
conda activate foobar-dashboard
conda env update -f environment.yml
```

## Quick Start

```
R -e "shiny::runApp('src/visualization/shiny/', port = 7890)"
# open the printed webaddress (http://127.0.0.1:7890)
```

## Build Docker image

```
docker build -t foobar-dashboard .
```

## Run Docker image

```
docker run foobar-dashboard
```

