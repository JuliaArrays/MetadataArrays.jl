module MetadataArrays

import ArrayInterfaceCore: parent_type, is_forwarding_wrapper, can_setindex,
    can_change_size, @assume_effects
using Base: Fix2, @propagate_inbounds
import DataAPI: metadata, metadatakeys #, metadatasupport
using LinearAlgebra

export
    MetadataArray,
    MetadataMatrix,
    MetadataVector,
    metadata,
    metadatakeys

include("interface.jl")

@nospecialize

struct MetadataArray{T, N, P<:AbstractArray,M<:MetadataNode} <: AbstractArray{T, N}
    parent::P
    metadata::M

    global _MDArray
    _MDArray(p, mdn::MetadataNode) = new{eltype(p),ndims(p),typeof(p),typeof(mdn)}(p, mdn)
    _MDArray(p, ::NoMetadata) = p
end

"""
    MetadataArray(parent::AbstractArray, metadata)

Custom `AbstractArray` object to store an `AbstractArray` `parent` as well as some `metadata`.

# Examples

```jldoctest metadataarray_docs
julia> v = ["John", "John", "Jane", "Louise"];

julia> mdv = MetadataArray(v, Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataArrays.MetadataArray{String,Dict{String,String},1,Array{String,1}}:
 "John"
 "John"
 "Jane"
 "Louise"

julia> metadata(mdv)
Dict{String,String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"

```
"""
function MetadataArray(p::AbstractArray, mdn::MetadataNode)
    check_metadata(p, mdn)
    _MDArray(p, mdn)
end
function MetadataArray(p::AbstractArray, md::NamedTuple)
    MetadataArray(p, MetadataNode{MetadataNamed}(md))
end
MetadataArray(p::AbstractArray, md) = MetadataArray(p, MetadataNode(md))
function MetadataArray(p::AbstractArray; kwargs...)
    MetadataArray(p, MetadataNode{MetadataNamed}(values(kwargs)))
end

"""
    MetadataMatrix

Shorthand for `MetadataVector{T,P<:AbstractArray{T,2},M}`.

See also: [`MetadataArray`](@ref), [`MetadataMatrix`](@ref)
"""
const MetadataMatrix{T,P<:AbstractArray{T,2},M} = MetadataArray{T, 2, P, M}
MetadataMatrix(m::AbstractMatrix, md) = MetadataArray(m, md)

"""
    MetadataVector

Shorthand for `MetadataVector{T,P<:AbstractArray{T,1},M}`.

See also: [`MetadataArray`](@ref), [`MetadataMatrix`](@ref)
"""
const MetadataVector{T,P<:AbstractArray{T,1},M} = MetadataArray{T, 1, P, M}
MetadataVector(v::AbstractVector, md) = MetadataArray(v, md)

# avoid new methods for every new parent, metadata type permutation
function Base.convert(T::Type{<:MetadataArray}, mda::MetadataArray)
    if isa(mda, T)
        mda
    else
        MetadataArray(convert(parent_type(T), parent(mda)), convert(_metatype(T), _meta(mda)))
    end
end

_metatype(T::Type{<:MetadataArray}) = fieldtype(T, :metadata)
_meta(mda::MetadataArray) = getfield(mda, :metadata)
_propagate(mda::MetadataArray) = propagate_metadata(_meta(mda))

Base.parent(mda::MetadataArray) = getfield(mda, :parent)
parent_type(T::Type{<:MetadataArray}) = fieldtype(T, :parent)

is_forwarding_wrapper(T::Type{<:MetadataArray}) = true
can_setindex(T::Type{<:MetadataArray}) = can_setindex(parent_type(T))
can_change_size(T::Type{<:MetadataArray}) = can_change_size(parent_type(T))

Base.IndexStyle(T::Type{<:MetadataArray}) = IndexStyle(fieldtype(T, :parent))

Base.iterate(mda::MetadataArray) = iterate(parent(mda))
Base.iterate(mda::MetadataArray, state) = iterate(parent(mda), state)

Base.first(mda::MetadataArray) = first(parent(mda))
Base.step(mda::MetadataArray) = step(parent(mda))
Base.last(mda::MetadataArray) = last(parent(mda))
Base.size(mda::MetadataArray) = size(parent(mda))
Base.axes(mda::MetadataArray) = axes(parent(mda))
Base.strides(mda::MetadataArray) = strides(parent(mda))

Base.length(mda::MetadataArray) = length(parent(mda))
Base.firstindex(mda::MetadataArray) = firstindex(parent(mda))
Base.lastindex(mda::MetadataArray) = lastindex(parent(mda))
Base.pointer(mda::MetadataArray) = pointer(parent(mda))

Base.in(val, mda::MetadataArray) = in(val, parent(mda))
Base.sizehint!(mda::MetadataArray, n::Integer) = sizehint!(parent(mda), n)
Base.keys(mda::MetadataArray) = keys(parent(mda))
Base.values(mda::MetadataArray) = values(parent(mda))
Base.isempty(mda::MetadataArray) = isempty(parent(mda))

Base.dataids(mda::MetadataArray) = (Base.dataids(parent(mda))..., Base.dataids(_meta(mda))...)

Base.propertynames(mda::MetadataArray) = keys(_meta(mda))
Base.getproperty(mda::MetadataArray, s::Symbol) = _meta(mda)[s]
Base.getproperty(mda::MetadataArray, s::String) = getproperty(mda, Symbol(s))
Base.hasproperty(mda::MetadataArray, s::Symbol) = haskey(_meta(mda), s)
Base.hasproperty(mda::MetadataArray, s::String) = hasproperty(mda, Symbol(s))

#region metadata methods

"""
    metadata(mda::MetadataArray)

Returns the raw metadata bound in `mda`. Currently, there are no guarantees that this will
return a collection whose values are correspond to the return values produced by
`metadata(mda, key)`.
"""
metadata(mda::MetadataArray) = _data(_meta(mda))
function metadata(mda::MetadataArray, key::AbstractString; style::Bool=false)
    metadata(mda, Symbol(key); style=style)
end
function metadata(mda::MetadataArray, key::Symbol; style::Bool=false)
    md = getproperty(mda, key)
    style ? (md, MetadataStyle(md)) : md
end
function metadata(mda::MetadataArray, key::AbstractString, default; style::Bool=false)
    metadata(mda, Symbol(key), default; style=style)
end
function metadata(mda::MetadataArray, key::Symbol, default; style::Bool=false)
    md = get(_meta(mda), key, default)
    style ? (md, MetadataStyle(md)) : md
end
metadatakeys(mda::MetadataArray) = keys(_meta(mda))
metadatasupport(T::Type{<:MetadataArray}) = (read=true, write=false)

#endregion
@specialize

@propagate_inbounds Base.getindex(mda::MetadataArray, i::Int...) = parent(mda)[i...]
@propagate_inbounds function Base.getindex(mda::MetadataArray, inds...)
    _MDArray(parent(mda)[inds...], _propagate(mda))
end
@propagate_inbounds function Base.setindex!(mda::MetadataArray, val, inds...)
    setindex!(parent(mda),val, inds...)
end

@inline function Base.similar(mda::MetadataArray)
    _MDArray(similar(parent(mda)), _propagate(mda))
end
@inline function Base.similar(mda::MetadataArray, ::Type{T}) where {T}
    _MDArray(similar(parent(mda), T), _propagate(mda))
end
# TODO address multi-dimensional metadata
function Base.similar(mda::MetadataArray, ::Type{T}, dims::Dims) where {T}
    _MDArray(similar(parent(mda), T, dims), _propagate(mda))
end
Base.reshape(mda::MetadataArray, d::Dims) = _MDArray(reshape(parent(mda), d), _meta(mda))

end # module
