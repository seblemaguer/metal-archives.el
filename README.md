# Metal-archives to org-agenda package

The goal of this package is to generate an org-file for the upcoming releases given by metal-archives.

## How to install

As the current packages depends on `om.el` (), it is suggested to use straight or quelpa to install it:

The minimum recipe using quelpa is the following one:

```elisp
(use-package metal-archives
    :ensure quelpa
    :quelpa (metal-archives :repo "seblemaguer/metal-archives.el" :fetcher github))
```

## Entry points

This packages provide four interactive functions:
  - `metal-archives-retrieve-next-releases`: synchronize the database of the upcoming releases
  - `metal-archives-org-generate-org-from-db`: generates an org file containing *all* the upcoming releases
  - `metal-archives-load-artists-map`: load the database of favorite artists (see ~FIXME~)
  - `metal-archives-shopping-list-update`: update the shopping list of the upcoming releases for the favorite artists (see ~FIXME~)

## Provided extended features

This package provides two extended features: an org interface and a shopping list management.

### Org interface

The org interface provides a convenient to generate a org file containing all the upcoming releases.
This file could be added in the org-agenda file list to monitor the releases

The org interface is controlled by:
  - the variable `metal-archives-org-target-file` which defines the file where *all* the upcoming releases;
  - the variable `metal-archives-org-template` which defines the template to represent a release node;
  - the function `metal-archives-org-generate-org-from-db` which is filling the `metal-archives-org-target-file` from the *already synchronized* release database.

### Shopping list

The shopping list management is a refinement of the org interface.
The idea of this extension is to provide a convenient way to store in a shopping list only releases of selected artits.

The shopping list management is controlled by:
  - the variable `metal-archives-shopping-list-target-file` which defines the file where the shopping list is stored;
  - the variable `metal-archives-shopping-list-root-node` which defines the title of the root node containing the shopping list;
  - the function `metal-archives-shopping-list-update` which updates the shopping list using a database (stored in `metal-archives-shopping-list-release-to-flush`) previously populated.

The variable `metal-archives-shopping-list-release-to-flush` is a list of releases as defined in the structure `metal-archives-entry` from metal-archives.el

Finally, to simplify the population of the shopping list, a hook as been defined and can be set to the `metal-archives-favorite-handle`.
This hook is named `metal-archives-shopping-list-add-release-and-alert`
