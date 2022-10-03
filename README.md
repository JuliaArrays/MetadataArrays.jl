# MetadataArrays

[![Build Status](https://travis-ci.org/JuliaArrays/MetadataArrays.jl.svg?branch=master)](https://travis-ci.org/JuliaArrays/MetadataArrays.jl)
[![codecov.io](http://codecov.io/github/JuliaArrays/MetadataArrays.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaArrays/MetadataArrays.jl?branch=master)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaArrays.github.io/MetadataArrays.jl/stable/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaArrays.github.io/MetadataArrays.jl/dev/)

Implementation of arrays with metadata.

## Metadata Arrays

`MetadataArray` binds an array to named metadata.

```julia
julia> v = ["John", "John", "Jane", "Louise"];

julia> mdv = MetadataArray(v, groups = Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataVector{String, Vector{String}, NamedTuple{(:groups,), Tuple{Dict{String, String}}}}:
 "John"
 "John"
 "Jane"
 "Louise"

julia> metadata(mdv, :groups)
Dict{String, String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"

```
