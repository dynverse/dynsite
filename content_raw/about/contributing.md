---
title: "Contributing"
---

We welcome any contributions to this project, be it changes in documentation, bug fixes, new functionality, or even new packages if they fit the scope of our project. When making big changes, please first discuss the change you wish to make via issue, email, or any other method with the owners of the repository.

## Pull Requests

* Make sure your code is following **[tidyverse style guide](https://style.tidyverse.org/)**
* Ensure that you are working in the **devel branch** (or any derivates of the devel branch)
* Edit the NEWS.md and **bump the version number** to x.x.x.9000 if there is not yet an unreleased version present
* Adding a unit test will increase the likelihood of acceptance

## Branches

Each package always has 2 branches: devel and master. All development happens in the devel branch (or a derivative thereof), and this is only merged once the features are tested thoroughly.  

The devel branch always has version number x.x.x.9000. This version number is bumped right before merging to master.

We have strict rules regarding dependencies between dynverse packages:

* devel branch always depends on the devel branch of other dynverse packages
* master branch always depends on the master branch of other dynverse packages
* cran versions (obivously) always depend on cran versions of other dynverse packages

The Remotes of a package are only removed from master right before submitting to CRAN.

To help development, we provide some helper functions in the `dynutils` package:

* `switch_devel()` will bump version number to x.x.x.9000 and will add @devel to Remotes
* `switch_master()` will bump version number to x.x.x and will add @master to Remotes
* `switch_cran()` will remove remotes
