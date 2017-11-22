
As the type of substitution and substitution_composition were left to you, you are required to create certain var declarations and 1 function to help with the demo.

Please note the following:
i. You will submit the following in a .ml file
ii. name of your file will be demo4_studentid.ml
iii. make sure your submission is free of any syntax errors as I would be "including" this file with "#use" during demo.
iv. It goes without that this submission carries no marks. It is only to help with the demos.

I]
create the following substitution_compositions formatted according the data type you have used for substitution composition:
(reaplce the italicized part with your code)

let sub1 = R/P ;;
let sub2 = g(Z,X)/Y ;;
let sub3 = 2/Y o Y/X ;;
let sub4 = *(Y,Y)/X o Z/Y ;;
let sub5 = M/X o N/Y o g(X,*(Y,*(X,Y)))/Z ;;
let sub6 = g(X,*(Y,*(X,Y)))/Z o M/X o N/Y ;;
let sub7 = I/X ;;
let sub8 = J/Y ;;

II]
create a function mgu_to_list to display your mgu in a human readable form. The function should convert your mgu to a substitution (<variable,term>pair) list.

val mgu_to_list = substitution_composition -> ('a * 'b) list = <fun>

                                       where 'a,'b represent a substitution pair 'b/'a

If your substitution_composition is already of the type ('a * 'b) list then the definition for function mgu_to_list would be:

                                      let mgu_to_list m = m;;
