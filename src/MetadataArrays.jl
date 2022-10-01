module MetadataArrays

import ArrayInterfaceCore: parent_type, is_forwarding_wrapper
import DataAPI: metadata, metadatakeys #, metadatasupport

export MetadataArray, MetadataVector, metadata

include("interface.jl")

@nospecialize

struct MetadataArray{T, N, P<:AbstractArray,M<:NamedTuple} <: AbstractArray{T, N}
    parent::P
    metadata::M

    global _MDArray(p, md) = new{eltype(p),ndims(p),typeof(p),typeof(md)}(p, md)
end

function MetadataArray(p::AbstractArray, md::NamedTuple)
    check_metadata(p, md)
    _MDArray(p, m)
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
MetadataVector(v::AbstractVector, md) = MetadataArray(v, n)

Base.parent(mda::MetadataArray) = getfield(mda, :parent)
parent_type(T::Type{<:MetadataArray}) = fieldtype(T, :parent)

is_forwarding_wrapper(T::Type{<:MetadataArray}) = true

#region property methods
Base.propertynames(mda::MetadataArray) = keys(metadata(mda))
Base.getproperty(mda::MetadataArray, s::Symbol) = getproperty(metadata(mda), s)
Base.setproperty!(mda::MetadataArray, s::Symbol, v) = setproperty!(metadata(mda), s, v)
Base.hasproperty(mda::MetadataArray, s::Symbol) = in(s, keys(mda))

Base.setproperty!(mda::MetadataArray, s::String, v) = setproperty!(mda, Symbol(s), v)
Base.getproperty(mda::MetadataArray, s::String) = getproperty(mda, Symbol(s))
Base.hasproperty(mda::MetadataArray, s::String) = hasproperty(mda, Symbol(s))
#endregion

#region metadata methods
"""
    metadata(mda::MetadataArray)

Returns the raw metadata bound in `mda`. Currently, there are no guarantees that this will
return a collection whose values are correspond to the return values produced by
`metadata(mda, key)`.
"""
metadata(mda::MetadataArray) = getfield(mda, :metadata)
function metadata(mda::MetadataArray, key::Symbol; style::Bool=false)
    md = getproperty(mda, key)
    style ? (md, MetadataStyle(md)) : md
end
function metadata(mda::MetadataArray, key::Symbol, default; style::Bool=false)
    md = get(metadata(mda), key, default)
    style ? (md, MetadataStyle(md)) : md
end
metadatasupport(T::Type{<:MetadataArray}) = (read=true, write=false)
#endregion

Base.size(mda::MetadataArray) = Base.size(getfield(mda, :parent))

Base.axes(mda::MetadataArray) = Base.axes(getfield(mda, :parent))

Base.IndexStyle(T::Type{<:MetadataArray}) = IndexStyle(fieldtype(T, :parent))

@specialize

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

Base.similar(A::MetadataArray, ::Type{S}, dims::Dims) where S =
    MetadataArray(similar(parent(A), S, dims), metadata(A))

function Base.reshape(s::MetadataArray, d::Dims)
    MetadataArray(reshape(parent(s), d), metadata(s))
end

end # module
