"""
    MetadataArray(parent::AbstractArray, metadata)

Custom `AbstractArray` object to store an `AbstractArray` `parent` as well as some `metadata`.

# Examples

```jldoctest metadataarray
julia> v = ["John", "John", "Jane", "Louise"];

julia> s = MetadataArray(v, Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataArrays.MetadataArray{String,Dict{String,String},1,Array{String,1}}:
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
"""
struct MetadataArray{T, N, M, S<:AbstractArray} <: AbstractArray{T, N}
    parent::S
    metadata::M
end

MetadataArray(v::AbstractArray{T, N}, m::M = ()) where {T, N, M} =
     MetadataArray{T, N, M, typeof(v)}(v, m)

"""
    MetadataVector{T, M, S<:AbstractArray}

Shorthand for `MetadataArray{T, 1, M, S}`.
"""
const MetadataVector{T, M, S<:AbstractArray} = MetadataArray{T, 1, M, S}

MetadataVector(v::AbstractVector, n = ()) = MetadataArray(v, n)

Base.size(s::MetadataArray) = Base.size(parent(s))

Base.axes(s::MetadataArray) = Base.axes(parent(s))

Base.IndexStyle(T::Type{<:MetadataArray}) = IndexStyle(_parent_type(T))

Base.@propagate_inbounds function Base.getindex(s::MetadataArray, x::Int...)
    @boundscheck checkbounds(parent(s), x...)
    @inbounds ret = getindex(parent(s), x...)
    return ret
end

Base.@propagate_inbounds function Base.setindex!(s::MetadataArray, val, x::Int...)
    @boundscheck checkbounds(parent(s), x...)
    @inbounds parent(s)[x...] = val
    return val
end

Base.parent(s::MetadataArray) = s.parent

_parent_type(::Type{MetadataArray{T, M, N, S}}) where {T,M,N,S} = S

"""
    metadata(s::MetadataArray)

Returns metadata for `s`.
"""
metadata(s::MetadataArray) = s.metadata

metadata(s::SubArray) = metadata(parent(s))

metadata(s::T) where {T<:AbstractArray} = nothing

Base.similar(A::MetadataArray, ::Type{S}, dims::Dims) where S =
    MetadataArray(similar(parent(A), S, dims), metadata(A))

function Base.reshape(s::MetadataArray, d::Dims)
    MetadataArray(reshape(parent(s), d), metadata(s))
end
