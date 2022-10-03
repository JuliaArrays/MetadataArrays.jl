
module MetadataInterface

import ArrayInterfaceCore: parent_type, is_forwarding_wrapper, can_setindex,
    can_change_size
using DataAPI
import DataAPI: metadata, metadatakeys, metadatasupport

@static if isdefined(Base, Symbol("@assume_effects"))
    using Base: @assume_effects
else
    macro assume_effects(_, ex)
        :(Base.@pure $(ex))
    end
end
@assume_effects :total function find_all_true(t::Tuple{Vararg{Bool}})
    out = Int[]
    for i in 1:nfields(t)
        if getfield(t, i)
            push!(out, i)
        end
    end
    (out...,)
end
@assume_effects :total function _find_first_symbol(s::Symbol, syms::Tuple{Vararg{Symbol}})
    for i in 1:nfields(syms)
        getfield(syms, i) === s && return i
    end
    return 0
end

@assume_effects :total function _merge(an::Tuple{Vararg{Symbol}}, bn::Tuple{Vararg{Symbol}})
    names = Symbol[an...]
    for n in bn
        if _find_first_symbol(n, an) === 0
            push!(names, n)
        end
    end
    (names...,)
end

struct Names{syms} <: AbstractVector{Symbol}
    Names(syms::Tuple{Vararg{Symbol}}) = new{syms}()
end
Base.Tuple(::Names{syms}) where {syms} = syms
Base.merge(@nospecialize(x::Names)) = x
function Base.merge(@nospecialize(x::Names), @nospecialize(y::Names))
    Names(_merge(Tuple(x), Tuple(y)))
end
@inline function Base.merge(@nospecialize(x::Names), @nospecialize(y::Names), zs::Names...)
    merge(merge(x, y), zs...)
end
Base.length(@nospecialize(n::Names)) = length(Tuple(n))
Base.size(@nospecialize(n::Names)) = (length(n),)
Base.firstindex(@nospecialize(n::Names)) = 1
Base.lastindex(@nospecialize(n::Names)) = length(n)
Base.@propagate_inbounds Base.getindex(@nospecialize(n::Names), i::Int) = getfield(Tuple(n), i)

"""
    UndefValue

Internal type used to indicate the absence of a value without throwing an error.

!!! warning
    This is not part of the public API and may change without notice.
"""
struct UndefValue end
const undefvalue = UndefValue()

#region MetadataStyle
"""
    MetadataStyle(M::Type)

Given the metadata type (`M`), returns a subtype of `MetadataStyle` that regulates how
instances of `M` are propagated between operations. New metadata styles should define:

* `propagate_metadata(style::NewStyle, md)`
* `combine_metadata(style::NewStyle, mds::Tuple)`

"""
abstract type MetadataStyle end
MetadataStyle(T::Type) = DefaultStyle()
MetadataStyle(::Type{Union{}}) = UnknownStyle()  # ambiguity resolution
MetadataStyle(@nospecialize T::Type{<:MetadataStyle}) = MetadataStyleStyle()

"""
    PersistentStyle

Subtype of `MetadataStyle` indicating that its associated metadata should persist
across operations along with its parent context.

See also: [`MetadataStyle`](@ref), [`DefaultStyle`](@ref)
"""
struct PersistentStyle <: MetadataStyle end

"""
    DefaultStyle

The default type returned by `MetadataStyle(type)`. Metadata accompanied by `DefaultStyle`
is not propagated.

See also: [`MetadataStyle`](@ref), [`PersistentStyle`](@ref)
"""
struct DefaultStyle <: MetadataStyle end

"""
    MetadataStyleStyle

Subtype of `MetadataStyle` for metadata that is itself a subtype of `MetadataStyle`.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataStyleStyle <: MetadataStyle end

"""
    NamedStyle

Subtype of `MetadataStyle` indicating that it's associated data is a `NamedTuple` where
each field is also metadata.

See also: [`MetadataStyle`](@ref)
"""
struct NamedStyle <: MetadataStyle end

struct UnknownStyle <: MetadataStyle end

# TODO expectations of AbstractDimensionStyle
# The accompanying metadata is expected to conform to the the standard array interface
"""
    AbstractDimensionStyle{N} <: MetadataStyle

Supertype for styles accompanying metadata associated with dimensions of its parent context.
`N` is the dimensionality of the parent context that associated metadata is characterizing.
Therefore, both the parent context and metadata should produce `N` when calling `ndims`.
Subtypes of `AbstractDimensionStyle` will propagate by default
"""
abstract type AbstractDimensionStyle{N} <: MetadataStyle end

"""
    AbstractIndicesStyle{N} <: AbstractDimensionStyle{N}

Supertype for styles whose metadata characterizes indices of their parent context. It is
expected that `axes(metadata) == axes(context)` is `true`.

See also: [`AbstractDimensionStyle`](@ref)
"""
abstract type AbstractIndicesStyle{N} <: MetadataStyle end

@noinline function _throw_axes(@nospecialize(s), @nospecialize(ctx), @nospecialize(md))
    throw(ArgumentError("""
MetadataStyle $(s) applied to metadata and parent context with unequal indices.
Got axes(metadata) = $(axes(md)) and axes(context) = $(axes(ctx))."""))
end
@noinline function _throw_ndims(n::Int, nctx::Int, nmd::Int)
    throw(ArgumentError("""
AbstractMetadataStyle with $(n) dimensions assigned to metadata with $(nmd) dimensions
and context with $(nctx) dimensions."""))
end
"""
    IndicesStyle{S,N}

A subtype of `AbstractDimensionStyle` indicating that metadata corresponds to the indices
of its parent context.

See also: [`AbstractDimensionStyle`](@ref), [`AbstractIndicesStyle`](@ref)
"""
struct IndicesStyle{S<:MetadataStyle,N} <: AbstractIndicesStyle{N} end

"""
    style_rule(s1::MetadataStyle, s2::MetadataStyle) -> MetadataStyle

Indicate how to resolve different `MetadataStyle`s. For example,
`MetadataStyle(::Primary, ::Secondary) = Primary()` would indicate that style
`Primary` has precedence over `Secondary`. You do not have to (and generally should not)
define both argument orders.
"""
style_rule(::S, ::S) where {S<:MetadataStyle} = S()
style_rule(x::DefaultStyle, ::DefaultStyle) = x
style_rule(x::MetadataStyle, ::DefaultStyle) = x
function style_rule(::IndicesStyle{S1,N}, ::IndicesStyle{S2,N}) where {S1,S2,N}
    style = style_rule(S1, S2)
    if isa(style, UnknownStyle)  # `UnknownStyle` overwrites everything
        style
    else
        IndicesStyle{typeof(style),N}()
    end
end
style_rule(::MetadataStyle, ::MetadataStyle) = UnknownStyle()
style_rule(@nospecialize(x::MetadataStyle)) = x

"""
    combine_styles(mds...) -> MetadataStyle

Performs `MetadataStyle(typeof(arg))` for each argument and combines them into a single
`MetadataStyle`, using `style_rule(style1, style2)` to repeatedly determine the
style until only one argument remains.

See also: [`style_rule`](@ref)
"""
combine_styles() = DefaultStyle()
combine_styles(x) = MetadataStyle(typeof(x))
combine_styles(x, y) = _combine_styles(combine_styles(x), combine_styles(y))
@inline function combine_styles(x, y, zs...)
    _combine_styles(combine_styles(x), combine_styles(y, zs...))
end
function _combine_styles(x::MetadataStyle, y::MetadataStyle)
    _catch_unkown(style_rule(x, y), style_rule(y, x), x, y)
end

## _catch_unkown
_catch_unkown(s1, s2::UnknownStyle, @nospecialize(x), @nospecialize(y)) = s1
_catch_unkown(s1::UnknownStyle, s2, @nospecialize(x), @nospecialize(y)) = s2
_catch_unkown(s1::S, s2::S, @nospecialize(x), @nospecialize(y)) where {S} = s1
@noinline function _catch_unkown(s1::UnknownStyle, s2::UnknownStyle, @nospecialize(x), @nospecialize(y))
    error("Failed to combine `MetadataStyle`s  $(x) and $(y))")
end
@noinline function _catch_unkown(@nospecialize(s1), @nospecialize(s2), @nospecialize(x), @nospecialize(y))
    error("""
conflicting MetadataStyle rules defined
  MetadataStyle(::$(typeof(x)), ::$(typeof(y))) = $(typeof(s1))()
  MetadataStyle(::$(typeof(y)), ::$(typeof(x))) = $(typeof(s2))()
One of these should be undefined (and thus return UnknownStyle).""")
end
#endregion

@nospecialize

"""
    MetadataNode{S<:MetadataStyle}(data[, context])

Dedicated node type for associating the style `S` with `data`. `context` may be provided,
describing the context in which `data` becomes metadata.
"""
struct MetadataNode{S<:MetadataStyle,M,C} <: AbstractVector{Any}
    metadata::M
    context::C

    MetadataNode{S}(data, ctx) where {S} = new{S,typeof(data),typeof(ctx)}(data, ctx)
    MetadataNode{S}(data) where {S} = MetadataNode{S}(data, undefvalue)
    MetadataNode(data, ctx) = MetadataNode{typeof(MetadataStyle(typeof(data)))}(data, ctx)
    MetadataNode(data) = MetadataNode{typeof(MetadataStyle(typeof(data)))}(data)
end

const NamedNodes{syms,T<:Tuple,C} = MetadataNode{NamedStyle,NamedTuple{syms,T},C}

context(mdn::MetadataNode) = getfield(mdn, :context)
context(md) = undefvalue

MetadataStyle(::Type{<:MetadataNode{S}}) where {S} = S()

Base.dataids(mdn::MetadataNode) = Base.dataids(metadata(mdn))

function Base.convert(T::Type{<:MetadataNode}, mdn::MetadataNode)
    if isa(mdn, T)
        mdn
    else
        MetadataNode{typeof(MetadataStyle(T))}(
            convert(fieldtype(T, :metadata), getfield(mdn, :metadata)),
            convert(fieldtype(T, :context), getfield(mdn, :context))
        )
    end
end

metadata(mdn::MetadataNode) = getfield(mdn, :metadata)
metadata(mdn::NamedNodes, s::Symbol) = getfield(metadata(mdn), s)
metadata(mdn::MetadataNode, ::Symbol, d) = d
metadata(mdn::NamedNodes, key::Symbol, d) = get(metadata(mdn), key, d)
metadatakeys(mdn::MetadataNode) = keys(trynames(mdn))
metadatasupport(T::Type{<:MetadataNode}) = (read=true, write=false)
function Base.show(io::IO, m::MIME"text/plain", mdn::MetadataNode)
    print(io, "MetadataNode{$(nameof(typeof(MetadataStyle(typeof(mdn)))))}")
    print(io, "(")
    Base.show(io, m, metadata(mdn))
    ctx = getfield(mdn, :context)
    if ctx === undefvalue
        print(io, ", ")
        show(io, m, ctx)
    end
    print(io, ")")
    nothing
end

isundefnode(x) = false
isundefnode(::MetadataNode{DefaultStyle,UndefValue}) = true

"""
    delete_metadata(data)

Returns an instance of `data` without any attached metadata, without mutating any data.
If `data` has no metadata attached, then `data` is returned unchanged.
"""
delete_metadata(data) = data
delete_metadata(mdn::MetadataNode) = MetadataNode(undefvalue, context(mdn))

# TODO doc-trynames
trynames(data) = trynames(typeof(data))
trynames(T::Type) = Names(())
trynames(T::Type{<:NamedTuple{syms}}) where {syms} = Names(syms)
trynames(T::Type{<:NamedNodes}) = trynames(fieldtype(T, :metadata))

@specialize

"""
    trymeta(ctx, key::Symbol)

Attempts to extract metadata from the context `ctx` that is associated with `key`.
If there is not metadata associated with `key`, or `ctx` isn't associated with any
metadata, then `MetadataNode(undefvalue, ctx)` is returned.

See also: [`MetadataNode`](@ref)
"""
trymeta(ctx, key::Symbol) = MetadataNode(undefvalue, ctx)
@inline function trymeta(mdn::NamedNodes, key::Symbol)
    md = get(metadata(mdn), key, undefvalue)
    md === undefvalue ? MetadataNode(undefvalue, mdn) : md
end

"""
    check_metadata(s::MetadataStyle, ctx, md)

Throws and error if the metadata `md` isn't valid metadata for the context `ctx` given
the style `s`. Otherwise, returns `nothing`. This method is often called when users
construct a new instance binding `ctx` to `md`. Subtypes of `MetadataStyle` with unique
requirements concerning metadata and its relationship to its parent context should
overload this method.
"""
function check_metadata(@nospecialize(ctx), @nospecialize(md))
    check_metadata(combine_styles(md), ctx, md)
end
check_metadata(::PersistentStyle, @nospecialize(ctx), @nospecialize(md)) = nothing
check_metadata(::DefaultStyle, @nospecialize(ctx), @nospecialize(md)) = nothing
check_metadata(::MetadataStyleStyle, @nospecialize(ctx), @nospecialize(md::MetadataStyle)) = nothing
# TODO errors for MetadataStyleStyle and NamedStyle with incorrect data types
function check_metadata(s::NamedStyle, @nospecialize(ctx), @nospecialize(mdn::MetadataNode))
    check_metadata(s, ctx, metadata(mdn))
end
function check_metadata(::NamedStyle, @nospecialize(ctx), @nospecialize(md::NamedTuple))
    _check(ctx, md)
end
function _check(ctx, md::NamedTuple)
    for i in 1:nfields(md)
        md_i = getfield(md, i)
        check_metadata(combine_styles(md_i), ctx, md_i)
    end
    nothing
end
function check_metadata(::AbstractDimensionStyle{N}, @nospecialize(ctx), @nospecialize(md)) where {N}
    (ndims(ctx) === ndims(md) === N) || _throw_ndims(N, ndims(ctx), ndims(md))
    nothing
end
function check_metadata(s::AbstractIndicesStyle{N}, @nospecialize(ctx), @nospecialize(md)) where {N}
    (ndims(ctx) === ndims(md) === N) || _throw_ndims(N, ndims(ctx), ndims(md))
    (axes(ctx) == axes(md)) || _throw_axes(s, ctx, md)
    nothing
end

"""
    propagate_metadata(style::MetadataStyle, md)

Simple method for propagating the metadata `md` based on `style`. Other methods responsible
for propagating metadata provide more information concerning the operation performed on the
metadata's parent context (such as [`permutedims_metadata`](@ref)). `propagate_metadata`
is the final method called when `style` isn't overloaded for methods providing more context.
"""
propagate_metadata(::PersistentStyle, @nospecialize(md)) = md
propagate_metadata(::MetadataStyleStyle, @nospecialize(md)) = md
propagate_metadata(::AbstractDimensionStyle, @nospecialize(md)) = md
propagate_metadata(::DefaultStyle, @nospecialize(md)) = undefvalue
function propagate_metadata(::NamedStyle, mdn::MetadataNode)
    propagate(propagate_metadata, metadata(mdn))
end
function propagate_metadata(::NamedStyle, @nospecialize(md::NamedTuple))
    _propagate(propagate_metadata, md)
end

"""
    permutedims_metadata(style::MetadataStyle, md[, perm::Tuple])

Provides a hook for managing metadata aligned to dimensions between dimension permuting
methods (`adjoint`, `transpose`, `permutedims`). If this method isn't overloaded for
the given `style`, `propagate_metadata(style, md)` is called.
"""
permutedims_metadata(s::MetadataStyle, @nospecialize(md)) = propagate_metadata(s, md)
permutedims_metadata(s::AbstractDimensionStyle, @nospecialize(md)) = permutedims(md)
permutedims_metadata(s::MetadataStyle, @nospecialize(md), perm::Tuple) = propagate_metadata(s, md)
permutedims_metadata(s::AbstractDimensionStyle, @nospecialize(md), perm::Tuple) = permutedims(md, perm)

"""
    combine_metadata(style::MetadataStyle, mds::Tuple)

Combines each metadata value of `mds` into a single value. `style` dictates how this is
accomplished. Each value of `mds` corresponds to the metadata found at a single key.
`MetadataNode(::UndefValue, context)` is used to represent metadata where
`metadata(context, key)` was not defined.

See also: [`UndefValue`](@ref), [`MetadataNode`](@ref)
"""
combine_metadata(::DefaultStyle, @nospecialize(mds::Tuple)) = undefvalue
combine_metadata(::MetadataStyleStyle, ::Tuple{}) = DefaultStyle()
combine_metadata(s::MetadataStyleStyle, mds::Tuple{Any,Vararg{Any}}) = combine_metadata(s, Base.tail(mds))
@inline function combine_metadata(s::MetadataStyleStyle, mds::Tuple{MetadataStyle,Vararg{Any}})
    style_rule(getfield(mds, 1), combine_metadata(s, Base.tail(mds)))
end
combine_metadata(::NamedStyle, @nospecialize(mds::Tuple)) = combine(combine_metadata, mds)
# use the same principles as merge here, take the leftmost existing value
combine_metadata(::PersistentStyle, ::Tuple{}) = undefvalue
function combine_metadata(::PersistentStyle, ns::Tuple{Any,Vararg{Any}})
    n = getfield(ns, 1)
    isundefnode(n) ? combine_metadata(PersistentStyle(), Base.tail(ns)) : n
end

# TODO doc propagate
@inline function propagate(f::Function, nt::NamedTuple{syms}, args...) where {syms}
    t = ntuple(Val{nfields(nt)}()) do i
        nt_i = getfield(nt, i)
        f(MetadataStyle(typeof(nt_i)), nt_i, args...)
    end
    inds = find_all_true(map(Base.Fix2(!==, undefvalue), t))
    if inds === ()
        return undefvalue
    else
        ninds = Val{nfields(inds)}()
        new_keys = ntuple(i -> getfield(syms, getfield(inds, i)), ninds)
        new_vals = ntuple(i -> getfield(t, getfield(inds, i)), ninds)
        return NamedTuple{new_keys}(new_vals)
    end
end

# TODO doc combine
function combine(f::Function, ctxs::Tuple)
    syms = Tuple(merge(map(trynames, ctxs)...))
    vn = Val{nfields(ctxs)}()
    t = ntuple(Val{nfields(syms)}()) do i
        key_i = getfield(syms, i)
        nodes_i = ntuple(j -> trymeta(getfield(ctxs, j), key_i), vn)
        f(combine_styles(nodes_i...), nodes_i)
    end
    inds = find_all_true(map(Base.Fix2(!==, undefvalue), t))
    if inds === ()
        return undefvalue
    else
        ninds = Val{nfields(inds)}()
        new_keys = ntuple(i -> getfield(syms, getfield(inds, i)), ninds)
        new_vals = ntuple(i -> getfield(t, getfield(inds, i)), ninds)
        return NamedTuple{new_keys}(new_vals)
    end
end

end
