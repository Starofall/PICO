dLength(H-_,L):- length(H,L).

diffToList(DList,List):- dappend(DList,[]-[],List-[]).

dappend(List1-Tail1, Tail1-Tail2, List1-Tail2).

addToDiffList(List-[Element| Tail2], Element, List-Tail2).

appendDiffList(List1-Tail1, Tail1-Tail2, List1-Tail2).
