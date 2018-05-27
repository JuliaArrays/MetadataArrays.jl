# MetadataArrays

Implementation of arrays with metadata.

## Metadata Arrays

A `MetadataArray` is a an `Array`, together with some metadata.

```julia
julia> v = ["John", "John", "Jane", "Louise"];

julia> s = MetadataArray(v, Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataArrays.MetadataArray{String,Dict{String,String},1,Array{String,1}}:
 "John"
 "John"
 "Jane"
 "Louise"
```

The parent `Array` as well as the metadata can be recovered with `parent` and `shifts` respectively.

```julia
julia> parent(s)
4-element Array{String,1}:
 "John"  
 "John"  
 "Jane"  
 "Louise"

julia> metadata(s)
Dict{String,String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"
```

`s` can be used as a regular `AbstractArray` (meaning all operations that work on `AbstractArray` should work on a `MetadataArray` out of the box:

```julia
julia> s[1] = "Jack";

julia> s[3]
"Jane"

julia> s
4-element MetadataArrays.MetadataArray{String,Dict{String,String},1,Array{String,1}}:
 "Jack"  
 "John"  
 "Jane"  
 "Louise"

julia> join(s)
 "JackJohnJaneLouise"
```
