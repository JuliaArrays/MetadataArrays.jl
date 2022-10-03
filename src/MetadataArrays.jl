module MetadataArrays

import ArrayInterfaceCore: parent_type, is_forwarding_wrapper, can_setindex,
    can_change_size, @assume_effects
using Base.Broadcast: BroadcastStyle
using LinearAlgebra

export
    MetadataArray,
    MetadataMatrix,
    MetadataVector,
    metadata,
    metadatakeys

include("MetadataInterface.jl")
using .MetadataInterface
import .MetadataInterface:
    MetadataStyle,
    MetadataDefault,
    MetadataPersistent,
    MetadataNamed,
    MetadataNode,
    UndefValue,
    # methods
    check_metadata,
    combine_metadata,
    delete_metadata,
    metadata,
    metadatakeys,
    metadatasupport,
    permutedims_metadata,
    propagate,
    propagate_metadata,
    trymeta,
    trynames,
    undefvalue

@nospecialize

struct MetadataArray{T, N, P<:AbstractArray,M<:NamedTuple} <: AbstractArray{T, N}
    parent::P
    metadata::M

    global _MDArray
    _MDArray(p, md) = new{eltype(p),ndims(p),typeof(p),typeof(md)}(p, md)
    _MDArray(p, ::UndefValue) = p
end

"""
    MetadataArray(parent::AbstractArray, metadata)

Custom `AbstractArray` object to store an `AbstractArray` `parent` as well as some `metadata`.

# Examples

```jldoctest metadataarray_docs
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
"""
function MetadataArray(p::AbstractArray, mdn::NamedTuple)
    check_metadata(MetadataNamed{keys(mdn)}(), p, mdn)
    _MDArray(p, mdn)
end
MetadataArray(p::AbstractArray, md) = MetadataArray(p, NamedTuple(md))
MetadataArray(p::AbstractArray; kwargs...) = MetadataArray(p, values(kwargs))

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
        MetadataArray(
            convert(parent_type(T), parent(mda)),
            convert(fieldtype(T, :metadata), metadata(mda))
        )
    end
end

_meta(mda::MetadataArray) = getfield(mda, :metadata)
_propagate(mda::MetadataArray) = propagate(propagate_metadata, _meta(mda))

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
Base.isempty(mda::MetadataArray) = isempty(parent(mda))

Base.dataids(mda::MetadataArray) = (Base.dataids(parent(mda))..., Base.dataids(_meta(mda))...)

Base.propertynames(mda::MetadataArray) = keys(_meta(mda))
Base.getproperty(mda::MetadataArray, s::Symbol) = _meta(mda)[s]
Base.getproperty(mda::MetadataArray, s::String) = getproperty(mda, Symbol(s))
Base.hasproperty(mda::MetadataArray, s::Symbol) = haskey(_meta(mda), s)
Base.hasproperty(mda::MetadataArray, s::String) = hasproperty(mda, Symbol(s))

"""
    metadata(mda::MetadataArray)

Returns the raw metadata bound in `mda`. Currently, there are no guarantees that this will
return a collection whose values are correspond to the return values produced by
`metadata(mda, key)`.
"""
metadata(mda::MetadataArray) = _meta(mda)
function metadata(mda::MetadataArray, key::AbstractString; style::Bool=false)
    metadata(mda, Symbol(key); style=style)
end
function metadata(mda::MetadataArray, key::Symbol; style::Bool=false)
    md = getproperty(mda, key)
    style ? (md, MetadataStyle(typeof(md))) : md
end
function metadata(mda::MetadataArray, key::AbstractString, default; style::Bool=false)
    metadata(mda, Symbol(key), default; style=style)
end
function metadata(mda::MetadataArray, key::Symbol, default; style::Bool=false)
    md = get(_meta(mda), key, default)
    style ? (md, MetadataStyle(typeof(md))) : md
end
metadatakeys(mda::MetadataArray) = keys(trynames(mda))
metadatasupport(T::Type{<:MetadataArray}) = (read=true, write=false)

delete_metadata(mda::MetadataArray) = parent(mda)

trynames(T::Type{<:MetadataArray}) = trynames(fieldtype(T, :metadata))
@inline function trymeta(mda::MetadataArray, key::Symbol)
    md = get(metadata(mda), key, undefvalue)
    md === undefvalue ? MetadataNode(undefvalue, mda) : md
end
@specialize

@inline function MetadataStyle(T::Type{<:MetadataArray})
    M = fieldtype(T, :metadata)
    if hasfield(M, :MetadataStyle)
        S = fieldtype(M, :MetadataStyle)
        return S <: MetadataStyle ? S() : MetadataDefault()
    else
        return MetadataDefault()
    end
end


Base.@propagate_inbounds Base.getindex(mda::MetadataArray, i::Int...) = parent(mda)[i...]
Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, inds...)
    _MDArray(parent(mda)[inds...], _propagate(mda))
end
Base.@propagate_inbounds function Base.setindex!(mda::MetadataArray, val, inds...)
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

for f in (:(Base.transpose), :(Base.adjoint), :(Base.permutedims), :(LinearAlgebra.pinv))
    @eval begin
        function $(f)(mda::MetadataVector)
            _MDArray($(f)(parent(mda)), propagate(permutedims_metadata, _meta(mda)))
        end
        function $(f)(mda::MetadataMatrix)
            _MDArray($(f)(parent(mda)), propagate(permutedims_metadata, _meta(mda)))
        end
    end
end
function Base.permutedims(mda::MetadataArray{T,N}, perm::NTuple{N,Int}) where {T,N}
    _MDArray(permutedims(parent(mda), perm), propagate(permutedims_metadata, _meta(mda), perm))
end

#region broadcast
"""
    MetadataArrayStyle{S}

Subtype of `BroadcastStyle` for `MetadataArray`, where `S` is the `BroadcastStyle`
of the parent array. This helps extract, combine, and propagate metadata from arrays.
"""
struct MetadataArrayStyle{S<:BroadcastStyle} <: Broadcast.AbstractArrayStyle{Any} end
MetadataArrayStyle(::S) where {S} = MetadataArrayStyle{S}()
MetadataArrayStyle(::S, ::Val{N}) where {S,N} = MetadataArrayStyle(S(Val(N)))
MetadataArrayStyle(::Val{N}) where {N} = MetadataArrayStyle{Broadcast.DefaultArrayStyle{N}}()
function MetadataArrayStyle(a::BroadcastStyle, b::BroadcastStyle)
    style = BroadcastStyle(a, b)
    if style === Broadcast.Unknown()
        return Broadcast.Unknown()
    else
        return MetadataArrayStyle(style)
    end
end
function Base.BroadcastStyle(T::Type{<:MetadataArray})
    MetadataArrayStyle{typeof(BroadcastStyle(parent_type(T)))}()
end
function Base.BroadcastStyle(::MetadataArrayStyle{A}, ::MetadataArrayStyle{B}) where {A,B}
    style = BroadcastStyle(A(), B())
    isa(style, Broadcast.Unknown) ? Broadcast.Unknown() : MetadataArrayStyle(style)
end

# Resolve ambiguities
# for all these cases, we define that we win to be the outer style regardless of order
for B in (:BroadcastStyle, :(Broadcast.DefaultArrayStyle), :(Broadcast.AbstractArrayStyle), :(Broadcast.Style{Tuple}),)
    @eval begin
        #Base.BroadcastStyle(::MetadataArrayStyle{A}, b::$B) where {A} = MetadataArrayStyle(A(), b)
        Base.BroadcastStyle(b::$B, ::MetadataArrayStyle{A}) where {A} = MetadataArrayStyle(b, A())
    end
end

# We need to implement copy because if the wrapper array type does not support setindex
# then the `similar` based default method will not work
function Broadcast.copy(bc::Broadcast.Broadcasted{MetadataArrayStyle{S}}) where {S}
    args = bc.args
    data = copy(Broadcast.Broadcasted{S}(bc.f, map(delete_metadata, args), axes(bc)))
    md = MetadataInterface.combine(combine_metadata, args)
    return _MDArray(data, md)
end
#endregion

end # module
