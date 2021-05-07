# GEOG5009M : Visualization for Geographic Data Science


This repository contains the code for generating [vis-for-gds](https://www.roger-beecham.com/vis-for-gds/).

The website uses the [Academic Hugo theme](https://sourcethemes.com/academic/), which is included as a submodule. When when cloning for the first time, use this command to bring down the theme as well:

    git clone --recursive https://github.com/gcushen/hugo-academic.git

To get the theme later, use this command:

    git submodule add \
      https://github.com/gcushen/hugo-academic.git \
      themes/hugo-academic

To update to the latest version of the theme, use:

    git submodule update --recursive --remote

The templates were taken from [Andrew Heiss's](https://www.andrewheiss.com) excellent [teaching websites](https://datavizf18.classes.andrewheiss.com/), with minimal configuring.
