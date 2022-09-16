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
struct MetadataArray{T, N, M, P<:AbstractArray} <: AbstractArray{T, N}
    parent::P
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

Base.size(mda::MetadataArray) = Base.size(getfield(mda, :parent))

Base.axes(mda::MetadataArray) = Base.axes(getfield(mda, :parent))

Base.IndexStyle(@nospecialize T::Type{<:MetadataArray}) = IndexStyle(fieldtype(T, :parent))

Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, x::Int...)
    @boundscheck checkbounds(parent(mda), x...)
    @inbounds ret = getindex(parent(mda), x...)
    return ret
end

Base.@propagate_inbounds function Base.setindex!(mda::MetadataArray, val, x::Int...)
    @boundscheck checkbounds(parent(mda), x...)
    @inbounds parent(mda)[x...] = val
    return val
end

Base.parent(@nospecialize mda::MetadataArray) = getfield(mda, :parent)

_parent_type(::Type{MetadataArray{T, M, N, P}}) where {T,M,N,P} = P

"""
    metadata(s::MetadataArray)

Returns metadata for `s`.
"""
metadata(@nospecialize mda::MetadataArray) = getfield(mda, :metadata)

metadata(mda::SubArray) = metadata(parent(mda))

metadata(s::T) where {T<:AbstractArray} = nothing

# property methods
function Base.propertynames(mda::MetadataArray)
    _merge_propertynames(metakeys(mda), propertynames(getfield(mda, :parent)))
end
@inline function Base.getproperty(mda::MetadataArray, k::Symbol)
    prop = getmeta(mda, k, mdna)
    prop === mdna ? getproperty(getfield(mda, :parent), k) : prop
end
@inline function Base.getproperty(mda::MetadataArray, k::String)
    prop = getmeta(mda, k, mdna)
    prop === mdna ? getproperty(getfield(mda, :parent), k) : prop
end
@inline function Base.setproperty!(mda::MetadataArray, k::Symbol, v)
    _setmeta!(getfield(mda, :metadata), k, v)
end
@inline function Base.setproperty!(mda::MetadataArray, k::String, v)
    _setmeta!(getfield(mda, :metadata), k, v)
end
function Base.hasproperty(mda::MetadataArray, key::Symbol)
    hasmetakey(mda, key) || hasproperty(getfield(mda, :parent), key)
end
function Base.hasproperty(mda::MetadataArray, key::String)
    hasmetakey(mda, key) || hasproperty(getfield(mda, :parent), key)
end

Base.similar(A::MetadataArray, ::Type{S}, dims::Dims) where S =
    MetadataArray(similar(parent(A), S, dims), metadata(A))

function Base.reshape(s::MetadataArray, d::Dims)
    MetadataArray(reshape(parent(s), d), metadata(s))
end
