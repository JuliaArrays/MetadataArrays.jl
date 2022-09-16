# MetadataArrays

[![Build Status](https://travis-ci.org/JuliaArrays/MetadataArrays.jl.svg?branch=master)](https://travis-ci.org/JuliaArrays/MetadataArrays.jl)
[![codecov.io](http://codecov.io/github/JuliaArrays/MetadataArrays.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaArrays/MetadataArrays.jl?branch=master)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaArrays.github.io/MetadataArrays.jl/stable/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaArrays.github.io/MetadataArrays.jl/dev/)

Implementation of arrays with metadata.

## Metadata Arrays

A `MetadataArray` is a an `Array`, together with some metadata.

```julia
julia> v = ["John", "John", "Jane", "Louise"];

julia> mdv = MetadataArray(v, Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataVector{String, Dict{String, String}, Vector{String}}:
 "John"
 "John"
 "Jane"
 "Louise"

```

The parent `AbstractArray` as well as the metadata can be recovered with `parent` and `metadata` respectively.

```julia
julia> parent(mdv)
4-element Vector{String}:
 "John"
 "John"
 "Jane"
 "Louise"

julia> metadata(mdv)
Dict{String, String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"

```

`metadata` is preserved when taking views:

```julia
julia> metadata(view(mdv, 1:2))
Dict{String, String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"
```

`mdv` can be used as a regular `AbstractArray` (meaning all operations that work on `AbstractArray` should work on a `MetadataArray` out of the box.
