Re-submission:

"Please omit 'for the user' in your description."

Fixed.

"What do you mean with 'auto-generates working skpr code'?
What is 'skpr code'?"

Clarified:  "Includes a Shiny graphical user interface that displays the underlying code used to create and evaluate the design to improve ease-of-use and enhance reproducibility." 

"Please write package names and software names in single quotes (e.g. 'skpr')."

Fixed.

"Can you provide some references in the 'Description' field of your DESCRIPTION file in the form
authors (year) <doi:...> or <arXiv:...>
(no space after 'doi:' and 'arXiv:')?"

The paper is still undergoing internal revisions and has not been released.

"Please add more small examples (executable in < 5sec per Rd-file) in your Rd-files."
Fixed: Added dontrun wrappers to some Monte Carlo examples to keep each file under 5 seconds. Added additional short examples. 


## Test environments
* local Windows install, R 3.3.3
* local OS X install, R 3.2 
* ubuntu 14.04 (on travis-ci), R 3.4.1 
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There are no NOTEs.
