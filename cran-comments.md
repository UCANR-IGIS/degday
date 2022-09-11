## Resubmission

This is a resubmission. In this version I have:

* added a trailing slash to the following URL in the README.Rmd and README.md:
     
  * <https://github.com/ucanr-igis/degday/>

## Resubmission

This is a resubmission. In this version I have:

* added a trailing slash to the following URL in the DESCRIPTION:
     
  * <https://github.com/ucanr-igis/degday/>
 
* committed the update to GitHub
 
* checked the package again using [GitHub Actions](https://github.com/UCANR-IGIS/degday/actions)

  * There were no ERRORs or WARNINGs, or NOTEs
     
* checked the package again on R-Hub by running `devtools::check_rhub()` 

  * There were no ERRORs or WARNINGs

  * R-Hub: Notes:
        
        Windows Server 2022, R-devel, 64 bit: 
          * 'Found detritus in the temp directory: Found the following files/directories: 'lastMiKTeXException'
          * I couldn't find any such file locally, think it was something that didn't get cleaned up on their server
        
        Linux: 
          * "Skipping checking HTML validation: no command 'tidy' found"
          * This sounds like an issue with their checking system


## First Submission: R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies

None. 



