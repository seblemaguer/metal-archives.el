[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Metal-archives to org-agenda package

melpa information: [![MELPA](https://melpa.org/packages/metal-archives-badge.svg)](https://melpa.org/#/metal-archives)

The goal of this package is to generate an org-file for the upcoming releases given by metal-archives.

## Dependencies

This package depends on the following packages:
  - `request`
  - `json`
  - `ht`
  - `alert`
  - `org-mode`
  - `org-ml` https://github.com/ndwarshuis/org-ml

## How to install

The minimum recipe using melpa is the following one:

```elisp
(use-package metal-archives :ensure t)
```

## Entry points

This packages provide four interactive functions:
  - `metal-archives-retrieve-next-releases`: synchronize the database of the upcoming releases
  - `metal-archives-org-generate-org-from-db`: generates an org file containing *all* the upcoming releases (see [here](#Org-interface))
  - `metal-archives-load-artists-map`: load the database of favorite artists
  - `metal-archives-shopping-list-update`: update the shopping list of the upcoming releases for the favorite artists (see [here](#Shopping-list))

## Favorite artists

A management of favorite artists is implemented in the package.
It consists of two parts:
  1. a database of favorite artits
  2. an handle which triggers a specific action when a release from the favorite artist is spotted.

The database is a hash table where the key of the element is the artist name and the value the priority stored in the `alert` format (the incremental priority list: `trivial`, `low`, `normal`, `moderate`, `high`, `urgent`).
The variable `metal-archives-artist-map-filename` defines the database file.
This file is assumed to be a Tab Separated Value (TSV) file containing two columns (artist, priority).
The helper  `metal-archives-load-artists-map` is provided to load this database.

The handle is defined by `metal-archives-favorite-handle`.
By default, it is set to `metal-archives-favorite-alert`.
Of course this handle can be customized.


## Provided extended features

This package provides two extended features: an org interface and a shopping list management.

### Org interface

This is defined in the file [metal-archives-org.el](metal-archives-org.el).

The org interface provides a convenient to generate a org file containing all the upcoming releases.
This file could be added in the org-agenda file list to monitor the releases

The org interface is controlled by:
  - the variable `metal-archives-org-target-file` which defines the file where *all* the upcoming releases;
  - the variable `metal-archives-org-template` which defines the template to represent a release node;
  - the function `metal-archives-org-generate-org-from-db` which is filling the `metal-archives-org-target-file` from the *already synchronized* release database.


### Shopping list

melpa information: [![MELPA](https://melpa.org/packages/metal-archives-shopping-list-badge.svg)](https://melpa.org/#/metal-archives-shopping-list)

This is defined in the file [metal-archives-shopping-list.el](metal-archives-shopping-list.el).

The shopping list management is a refinement of the org interface.
The idea of this extension is to provide a convenient way to store in a shopping list only releases of selected artits.

The shopping list management is controlled by:
  - the variable `metal-archives-shopping-list-target-file` which defines the file where the shopping list is stored;
  - the variable `metal-archives-shopping-list-root-node` which defines the title of the root node containing the shopping list;
  - the function `metal-archives-shopping-list-update` which updates the shopping list using a database (stored in `metal-archives-shopping-list-release-to-flush`) previously populated.

The variable `metal-archives-shopping-list-release-to-flush` is a list of releases as defined in the structure `metal-archives-entry` from [metal-archives.el](metal-archives.el).

For perfomance constrained, it is advises to select a dedicated shopping list target file as the whole file is currently parsed.

Finally, to simplify the population of the shopping list, a hook as been defined and can be set to the `metal-archives-favorite-handle`.
This hook is named `metal-archives-shopping-list-add-release-and-alert`


## Setup example

A close setup to what I am using is the following:

```elisp
(use-package metal-archives-shopping-list
  :ensure t
  :commands (metal-archives-shopping-list-update)
  :hook
  (kill-emacs . metal-archives-shopping-list-update)
  (after-init . metal-archives-load-artists-map)

  :init
  (add-to-list 'org-agenda-files metal-archives-shopping-list-target-file)

  :config
  (setq metal-archives-favorite-handle 'metal-archives-shopping-list-add-release-and-alert))
```
