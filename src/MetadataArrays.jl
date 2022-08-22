module MetadataArrays

using ArrayInterface
using ArrayInterface: parent_type, known_first, known_last, known_step, StaticInt, to_dims,
    axes_types, known_dimnames, dimnames, has_dimnames
using Base: @propagate_inbounds, OneTo, tail
using Compat
using Compat: Returns
using LinearAlgebra
using Statistics
using Static
# import DataAPI: metadata
export MetadataArray, MetadataVector, metadata

@nospecialize

struct MetadataArray{T,N,
    P<:AbstractArray{T,N},
    M<:Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},
    D<:Tuple{Vararg{<:Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},N}},
    A<:Tuple{Vararg{<:Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},N}},
} <: AbstractArray{T, N}
    parent::P
    metadata::M
    dimmeta::D
    axismeta::A

global function _MetadataArray(p, m, d, a)
        new{eltype(p),ndims(p),typeof(p),typeof(m),typeof(d),typeof(a)}(p, m, d, a)
    end
end

function MetadataArray(
    p::AbstractArray{T,N},
    m::Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},
    d::Tuple{Vararg{<:Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},N}},
    a::Tuple{Vararg{<:Union{AbstractDict{String},AbstractDict{Symbol},NamedTuple},N}}
) where {T,N}
    _check_axismeta(p, a)
    _MetadataArray(p, m, d, a)
end

"""
    MetadataVector{T, M, S<:AbstractArray}

Shorthand for `MetadataArray{T, 1, M, S}`.
"""
const MetadataVector{T,P,M,D,A} = MetadataArray{T,1,P,M,D,A}

MetadataVector(v::AbstractVector, n = ()) = MetadataArray(v, n)

# MetadataMatrix
const MetadataMatrix{T,P,M,D,A} = MetadataArray{T,2,P,M,D,A}

Base.parent(x::MetadataArray) = getfield(x, :parent)

metadata(A::MetadataArray) = getfield(A, :metadata)
metadata(A::MetadataArray, key) = _get(getfield(A, :metadata), key)

""" dimmeta """
dimmeta(A::MetadataArray) = getfield(A, :dimmeta)
@inline dimmeta(A::MetadataArray, dim::Int) = getfield(getfield(A, :dimmeta), dim)
@inline function dimmeta(A::MetadataArray, ::StaticInt{dim}) where {dim}
    getfield(getfield(A, :dimmeta), dim)
end
@inline dimmeta(A::MetadataArray, dim, key) = _get(dimmeta(A, dim), key)

""" axismeta """
axismeta(A::MetadataArray) = getfield(A, :axismeta)
@inline axismeta(A::MetadataArray, dim::Int) = getfield(axismeta(A), dim)
@inline function axismeta(A::MetadataArray, ::StaticInt{dim}) where {dim}
    getfield(getfield(A, :axismeta), dim)
end
@inline axismeta(A::MetadataArray, dim, key) = _get(axismeta(A, dim), key)


@inline _get(d::AbstractDict{String}, key::Symbol) = d[String(key)]
@inline _get(d::AbstractDict{Symbol}, key) = d[Symbol(key)]
@inline _get(d::AbstractDict{String}, key) = d[key]
@inline _get(d::NamedTuple, key::AbstractString) = getfield(d, Symbol(key))
@inline _get(d::NamedTuple, key::Symbol) = getfield(d, key)
@inline _get(d::NamedTuple, ::StaticSymbol{key}) where {key} = getfield(d, key)
function _default_dims_wise_meta(A::AbstractArray)
    ntuple(Returns(NamedTuple{(),Tuple{}}(())), Val{ndims(A)}())
end
_check_selfmeta(x::AbstractArray, md::Union{NamedTuple,AbstractDict}) = nothing
function _check_dimmeta(x::AbstractArray, md::Tuple{Vararg{<:Union{NamedTuple,AbstractDict}}})
    ndims(x) === length(eltype(md)) && return nothing
    error("Number of array dimensions does not match number of dimmeta.")
end
function _check_axismeta(x::AbstractArray, md::Tuple{Vararg{<:Union{NamedTuple,AbstractDict}}})
    axs = axes(x)
    for i in 1:ndims(x)
        axs_i = axs[i]
        md_i = md[i]
        for (k,v) in pairs(md_i)
            if eachindex(IndexLinear(), v) != axs_i
                error("axismeta along dimension $(i) corresponding to key $(k) does not have the same indices as its corresponding array axis.")
            end
        end
    end
    return nothing
end

# _copymeta is the private method that iterates over collections of metadata
_copymeta(nt::NamedTuple) = NamedTuple{keys(nt)}(map(copymeta, nt))
_copymeta(d::AbstractDict) = copy(d)
# TODO individual metadata copying
copymeta(x) = copy(x)

_reducemeta(nt::NamedTuple) = NamedTuple{keys(nt)}(map(reducemeta, nt))
_reducemeta(d::AbstractDict) = Dict([k => reducemeta(v) for (k,v) in d])
# TODO individual metadata reduction
reducemeta(x) = x[1:1]

_known_keys(@nospecialize T::Type{<:NamedTuple}) = fieldnames(T)
_known_keys(@nospecialize T::Type) = nothing

_known_metadata(@nospecialize(T::Type{<:NamedTuple}), k::Symbol) = fieldtype(T, k)

@inline function pushmeta(A::MetadataArray, key::Symbol, md)
    _MetadataArray(parent(A), Base.setindex(metadata(A), md, key), dimmeta(A), axismeta(A))
end
@inline function pushdimmeta(A::MetadataArray, dim::Int, key::Symbol, md)
    _MetadataArray(parent(A), metadata(A),
        ntuple(Val{ndims(A)}()) do dim_i
            dim_i === dim ? Base.setindex(dimmeta(A, dim_i), key, md) : dimmeta(A, dim_i)
        end,
        axismeta(A)
    )
end
# TODO check axis length stuff
@inline function pushaxismeta(A::MetadataArray, dim::Int, key::Symbol, md)
    _MetadataArray(parent(A), metadata(A), dimmeta(A),
        ntuple(Val{ndims(A)}()) do dim_i
            dim_i === dim ? Base.setindex(axismeta(A, dim_i), key, md) : axismeta(A, dim_i)
        end
    )
end

function Base.setindex!(A::MetadataArray, vals, args...)
    inds = ArrayInterface.to_indices(A, args)
    @boundscheck checkbounds(A, inds...)
    @inbounds(setindex!(parent(A), vals, inds...))
end

@inline function Base.accumulate(op, A::MetadataArray; dims=nothing, kw...)
    _MetArray(
        accumulate(op, parent(A); dims= dims === nothing ? nothing : to_dims(A, dims), kw...),
        _copymeta(metadata(A)),
        map(_copymeta, dimmeta(A)),
        map(_copymeta, axismeta(A)),
    )
end
@inline function Base.cumsum(op, A::MetadataArray; dims, kw...)
    _MetArray(
        cumsum(op, parent(A); dims=to_dims(A, dims), kw...),
        _copymeta(metadata(A)),
        map(_copymeta, dimmeta(A)),
        map(_copymeta, axismeta(A)),
    )
end
@inline function Base.cumprod(op, A::MetadataArray; dims, kw...)
    _MetArray(
        cumprod(op, parent(A); dims=to_dims(A, dims), kw...),
        _copymeta(metadata(A)),
        map(_copymeta, dimmeta(A)),
        map(_copymeta, axismeta(A)),
    )
end
function LinearAlgebra.transpose(x::MetadataVector)
    _MetadataArray(
        LinearAlgebra.transpose(parent(x)),
        metadata(x),
        (NamedTuple{(),Tuple{}}(()), getfield(dimmeta(x), 1)),
        (NamedTuple{(),Tuple{}}(()), getfield(axismeta(x), 1)),
    )
end
function Base.adjoint(x::MetadataVector)
    _MetadataArray(
        Base.adjoint(parent(x)),
        metadata(x),
        (NamedTuple{(),Tuple{}}(()), getfield(dimmeta(x), 1)),
        (NamedTuple{(),Tuple{}}(()), getfield(axismeta(x), 1)),
    )
end
function Base.permutedims(x::MetadataVector)
    _MetadataArray(
        Base.permutedims(parent(x)),
        metadata(x),
        (NamedTuple{(),Tuple{}}(()), getfield(dimmeta(x), 1)),
        (NamedTuple{(),Tuple{}}(()), getfield(axismeta(x), 1)),
    )
end
function LinearAlgebra.transpose(x::MetadataMatrix)
    d1, d2 = dimmeta(x)
    a1, a2 = axismeta(x)
    _MetadataArray(LinearAlgebra.transpose(parent(x)), metadata(x), (d2, d1), (a2, a1))
end
function Base.adjoint(x::MetadataMatrix)
    d1, d2 = dimmeta(x)
    a1, a2 = axismeta(x)
    _MetadataArray(Base.adjoint(parent(x)), metadata(x), (d2, d1), (a2, a1))
end
function Base.permutedims(x::MetadataMatrix)
    d1, d2 = dimmeta(x)
    a1, a2 = axismeta(x)
    _MetadataArray(Base.permutedims(parent(x)), metadata(x), (d2, d1), (a2, a1))
end
@inline function Base.permutedims(x::MetadataArray{T,N}, perm::NTuple{N,Int}) where {T,N}
    d = dimmeta(x)
    a = axismeta(x)
    _MetadataArray(
        permutedims(parent(x), perm),
        ntuple(i -> getfield(d, i), Val{N}()),
        ntuple(i -> getfield(a, i), Val{N}()),
    )
end
Base.@propagate_inbounds function Base.getindex(A::MetadataArray, inds::Vararg{Union{Integer,StaticInt}})
    parent(A)[inds...]
end
Base.@propagate_inbounds function Base.getindex(A::MetadataArray; kwargs...)
    if isempty(kwargs)
        return parent(A)[]
    else
        return getindex(A, ArrayInterface.find_all_dimnames(dimnames(A), static(keys(kwargs)), Tuple(values(kwargs)), :))
    end
end
@inline function Base.getindex(A::MetadataArray, args::Vararg{Any})
    inds = ArrayInterface.to_indices(A, args)
    @boundscheck checkbounds(A, inds...)
    p = @inbounds(parent(A)[inds...])
    m = _copymeta(metadata(A))
    d, a = _index_dims_axis_meta(ArrayInterface.IndicesInfo{ndims(A)}(typeof(inds)), dimmeta(A), axismeta(A), inds)
    _MetadataArray(p, m, d, a)
end
@inline function Base.view(A::MetadataArray, args...)
    inds = ArrayInterface.to_indices(A, args)
    @boundscheck checkbounds(A, inds...)
    inds = ArrayInterface.to_indices(A, args)
    @boundscheck checkbounds(A, inds...)
    p = @inbounds(view(A, inds...))
    m = _copymeta(metadata(A))
    d, a = _index_dims_axis_meta(ArrayInterface.IndicesInfo{ndims(A)}(typeof(inds)), dimmeta(A), axismeta(A), inds)
    _MetadataArray(p, m, d, a)
end
Base.@propagate_inbounds function Base.view(A::MetadataArray; kwargs...)
    if isempty(kwargs)
        return _MetadataArray(view(parent(A)), metadata(A), dimmeta(A), axismeta(A))
    else
        return view(A, ArrayInterface.find_all_dimnames(dimnames(A), static(keys(kwargs)), Tuple(values(kwargs)), :))
    end
end
# pdims are mappings from index positions to parent dimensions
# cdims are mappings from index positions to child dimensions
@inline function _index_dims_axis_meta(
    ::ArrayInterface.IndicesInfo{N,pdims, cdims},
    d::Tuple,    # dimmeta
    a::Tuple,    # axismeta
    inds::Tuple  # will be same length as `pdims` and `cdims`
) where {N,pdims, cdims}
    dout = ntuple(Val{nfields(d)}()) do i
        pdim_i = getfield(pdims, i)
        cdim_i = getfield(cdims, i)
        if cdim_i isa Int  # can only propagate dimmeta with one to on corresponds to parent dim
            pdim_i isa Int ? getfield(d, pdim_i) : NamedTuple{(),Tuple{}}(())
        else
            ntuple(Returns(NamedTuple{(),Tuple{}}(())), length(cdim_i))
        end
    end
    aout = ntuple(Val{nfields(d)}()) do i
        pdim_i = getfield(pdims, i)
        cdim_i = getfield(cdims, i)
        if cdim_i isa Int  # can only propagate dimmeta with one to on corresponds to parent dim
            pdim_i isa Int ? @inbounds(getfield(a, pdim_i)[getfield(inds, i)]) : NamedTuple{(),Tuple{}}(())
        else
            ntuple(Returns(NamedTuple{(),Tuple{}}(())), length(cdim_i))
        end
    end
    dout, aout
end

function Base.mapreduce(f1, f2, A::MetadataArray; dims=:, init=Base._InitialValue())
    if dims === (:)
        mapreduce(f1, f2, parent(A); dims=dims, init=init)
    else
        d = to_dim(A, dims)
        if d isa Tuple
            a = ntuple(i->i in d ? _reducemeta(axismeta(A, i)) : axismeta(A, i), Val{ndims(A)}())
        else
            a = ntuple(i->i == d ? _reducemeta(axismeta(A, i)) : axismeta(A, i), Val{ndims(A)}())
        end
        _MetArray(
            mapreduce(f1, f2, parent(A); dims=d, init=init),
            _copymeta(metadata(A)),
            map(_copymeta, dimmeta(A)),
            a
        )
    end
end
@inline function Base.similar(A::MetadataArray)
    _MetArray(
        similar(parent(A)),
        _copymeta(metadata(A)),
        map(_copymeta, dimmeta(A)),
        map(_copymeta, axismeta(A))
    )
end
@inline function Base.similar(A::MetadataArray, ::Type{T}) where {T}
    _MetArray(
        similar(parent(A), T),
        _copymeta(metadata(A)),
        map(_copymeta, dimmeta(A)),
        map(_copymeta, axismeta(A))
    )
end

ArrayInterface.is_forwarding_wrapper(T::Type{<:MetadataArray}) = true
function ArrayInterface.defines_strides(T::Type{<:MetadataArray})
    ArrayInterface.defines_strides(parent_type(T))
end
function ArrayInterface.contiguous_axis(T::Type{<:MetadataArray})
    ArrayInterface.contiguous_axis(parent_type(T))
end
function ArrayInterface.contiguous_axis_indicator(T::Type{<:MetadataArray})
    ArrayInterface.contiguous_axis_indicator(parent_type(T))
end
function ArrayInterface.contiguous_batch_size(T::Type{<:MetadataArray})
    ArrayInterface.contiguous_batch_size(parent_type(T))
end
function ArrayInterface.known_length(T::Type{<:MetadataArray})
    ArrayInterface.known_length(parent_type(T))
end
function ArrayInterface.known_size(T::Type{<:MetadataArray})
    ArrayInterface.known_size(parent_type(T))
end
function ArrayInterface.known_first(T::Type{<:MetadataArray})
    ArrayInterface.known_first(parent_type(T))
end
function ArrayInterface.known_step(T::Type{<:MetadataArray})
    ArrayInterface.known_step(parent_type(T))
end
function ArrayInterface.known_last(T::Type{<:MetadataArray})
    ArrayInterface.known_last(parent_type(T))
end
function ArrayInterface.known_offsets(T::Type{<:MetadataArray})
    ArrayInterface.known_offsets(parent_type(T))
end
function ArrayInterface.known_strides(T::Type{<:MetadataArray})
    ArrayInterface.known_strides(parent_type(T))
end
function ArrayInterface.stride_rank(T::Type{<:MetadataArray})
end
function ArrayInterface.axes_types(T::Type{<:MetadataArray})
end
function ArrayInterface.can_setindex(T::Type{<:MetadataArray})
    ArrayInterface.can_setindex(parent_type(T))
end
function ArrayInterface.can_change_size(T::Type{<:MetadataArray})
    ArrayInterface.can_change_size(parent_type(T))
end
Base.eltype(T::Type{<:MetadataArray}) = Base.eltype(parent_type(T))
Base.valtype(T::Type{<:MetadataArray}) = Base.valtype(parent_type(T))
Base.keytype(T::Type{<:MetadataArray}) = Base.keytype(parent_type(T))
Base.IndexStyle(T::Type{<:MetadataArray}) = Base.IndexStyle(parent_type(T))
Base.IteratorSize(T::Type{<:MetadataArray}) = Base.IteratorSize(parent_type(T))

Base.copy(x::MetadataArray) = deepcopy(x)

ArrayInterface.parent_type(T::Type{<:MetadataArray}) = fieldtype(T, :parent)

Base.dataids(x::MetadataArray) = (Base.dataids(parent(x))..., Base.dataids(metadata(x))...)

Base.only(x::MetadataArray) = only(parent(x))
Base.findfirst(f::Union{Function,Type}, x::MetadataArray) = findfirst(f, parent(x))
Base.findlast(f::Union{Function,Type}, x::MetadataArray) = findlast(f, parent(x))

ArrayInterface.axes(x::MetadataArray) = ArrayInterface.axes(parent(x))
ArrayInterface.size(x::MetadataArray) = ArrayInterface.size(parent(x))
ArrayInterface.offsets(x::MetadataArray) = ArrayInterface.offsets(parent(x))
ArrayInterface.length(x::MetadataArray) = ArrayInterface.length(parent(x))
ArrayInterface.strides(x::MetadataArray) = ArrayInterface.strides(parent(x))

Base.axes(x::MetadataArray) = axes(parent(x))
@inline Base.axes(x::MetadataArray, dim::Symbol) = axes(parent(x), to_dims(x, dim))

Base.strides(x::MetadataArray) = strides(parent(x))
@inline Base.stride(x::MetadataArray, dim::Symbol) = Base.stride(parent(x), to_dims(x, dim))

Base.iterate(x::MetadataArray) = iterate(parent(x))
Base.iterate(x::MetadataArray, state) = iterate(parent(x), state)

Base.first(x::MetadataArray) = first(parent(x))
Base.step(x::MetadataArray) = step(parent(x))
Base.last(x::MetadataArray) = last(parent(x))
Base.size(x::MetadataArray) = size(parent(x))
Base.size(x::MetadataArray, dim::Symbol) = size(parent(x), to_dims(x, dim))

Base.length(x::MetadataArray) = length(parent(x))
Base.firstindex(x::MetadataArray) = firstindex(parent(x))
Base.lastindex(x::MetadataArray) = lastindex(parent(x))
Base.pointer(x::MetadataArray) = pointer(parent(x))

Base.eachindex(x::MetadataArray) = eachindex(parent(x))
Base.eachindex(S::IndexLinear, x::MetadataArray) = eachindex(S, parent(x))
Base.eachindex(S::IndexCartesian, x::MetadataArray) = eachindex(S, parent(x))

Base.in(val, x::MetadataArray) = in(val, parent(x))
Base.sizehint!(x::MetadataArray, n::Integer) = sizehint!(parent(x), n)
Base.keys(x::MetadataArray) = keys(parent(x))
Base.values(x::MetadataArray) = values(parent(x))
Base.isempty(x::MetadataArray) = isempty(parent(x))

@specialize


end # module
