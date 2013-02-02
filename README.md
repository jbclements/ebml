# EBML

A racket library to read and write EBML, the "extended binary markup language."

EBML is a packed format for representing structured data.

```
element = header data

header = encoded-id encoded-len

data = element ...
     | non-element
```


It's got at least one big problem, which is that the packed representation is ambiguous. Specifically, there's no way to reliably distinguish data that is a sequence of elements from data that is a binary blob.

