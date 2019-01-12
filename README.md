# Metal-archives to org-agenda package

The goal of this package is to generate an org-file for the upcoming releases given by metal-archives.

## How to install

For now, I suggest to use quelpa coupled with:

```elisp
(use-package ma2oa
    :ensure quelpa
    :quelpa (ma2oa :repo "seblemaguer/ma2oa.el" :fetcher github)
    )
```
