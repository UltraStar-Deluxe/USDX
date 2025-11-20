# Pipeline
This document lists some quirks and how-to's about the pipeline.

## Windows CI fails to download DLLs
It fails with a message that it can't find some zip file, and a little further up is `HTTP Error 401: Unauthorized`.

The pipeline uses a [fine-grained personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-fine-grained-personal-access-token) which expire at some point.
To get a new one:
* Go to [Settings - Personal access tokes](https://github.com/settings/personal-access-tokens)
* Resource owner = UltraStar-Deluxe
* Repository access = Only select repositories (`mxe`)
* Permissions: readonly for Actions on the `mxe` repository
* And then in the [USDX repository actions secrets](https://github.com/UltraStar-Deluxe/USDX/settings/secrets/actions) the token needs to be a `Repository secret` named `MXEACTIONSREADACCESSTOKEN`
