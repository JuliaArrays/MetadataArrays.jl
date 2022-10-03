
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

@assume_effects :total function _merge_names(an::Tuple{Vararg{Symbol}}, bn::Tuple{Vararg{Symbol}})
    names = Symbol[an...]
    for n in bn
        if _find_first_symbol(n, an) === 0
            push!(names, n)
        end
    end
    (names...,)
end

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
instances of `M` are propagated between operations.
"""
abstract type MetadataStyle end
MetadataStyle(T::Type) = DefaultStyle()
MetadataStyle(::Type{Union{}}) = MetadataUnknown()  # ambiguity resolution
MetadataStyle(@nospecialize T::Type{<:MetadataStyle}) = MetadataStyleStyle()

"""
    PersistentStyle

Subtype of `MetadataStyle` indicated that its associated metadata should persist
across operations that copy its contextual data.

See also: [`MetadataStyle`](@ref)
"""
struct PersistentStyle <: MetadataStyle end

"""
    DefaultStyle

The default type returned by `MetadataStyle(type)`.

See also: [`MetadataStyle`](@ref)
"""
struct DefaultStyle <: MetadataStyle end

"""
    MetadataStyleStyle

Subtype of `MetadataStyle` for metadata that is itself a subtype of `MetadataStyle`.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataStyleStyle <: MetadataStyle end

"""
    NamedStyle{names}

Subtype of `MetadataStyle` indicating that it's associated data is a `NamedTuple` where
each field is also metadata.

See also: [`MetadataStyle`](@ref)
"""
struct NamedStyle{nms} <: MetadataStyle end
Base.keys(::NamedStyle{nms}) where {nms} = nms

struct MetadataUnknown <: MetadataStyle end

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
style_rule(::MetadataStyle, ::MetadataStyle) = MetadataUnknown()
function style_rule(::NamedStyle{x}, ::NamedStyle{y}) where {x,y}
    NamedStyle{_merge_names(x, y)}()
end
style_rule(@nospecialize(x::MetadataStyle)) = x
@inline function style_rule(x::MetadataStyle, y::MetadataStyle, zs::MetadataStyle...)
    style_rule(style_rule(x, y), zs...)
end

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
_catch_unkown(s1, s2::MetadataUnknown, @nospecialize(x), @nospecialize(y)) = s1
_catch_unkown(s1::MetadataUnknown, s2, @nospecialize(x), @nospecialize(y)) = s2
_catch_unkown(s1::S, s2::S, @nospecialize(x), @nospecialize(y)) where {S} = s1
@noinline function _catch_unkown(s1::MetadataUnknown, s2::MetadataUnknown, @nospecialize(x), @nospecialize(y))
    error("Failed to combine `MetadataStyle`s  $(x) and $(y))")
end
@noinline function _catch_unkown(@nospecialize(s1), @nospecialize(s2), @nospecialize(x), @nospecialize(y))
    error("""
conflicting MetadataStyle rules defined
  MetadataStyle(::$(typeof(x)), ::$(typeof(y))) = $(typeof(s1))()
  MetadataStyle(::$(typeof(y)), ::$(typeof(x))) = $(typeof(s2))()
One of these should be undefined (and thus return MetadataUnknown).""")
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

const NamedNodes{syms,T<:Tuple,C} = MetadataNode{NamedStyle{syms},NamedTuple{syms,T},C}

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
trynames(T::Type) = NamedStyle{()}()
trynames(T::Type{<:NamedTuple{syms}}) where {syms} = NamedStyle{syms}()
trynames(T::Type{<:NamedNodes}) = trynames(fieldtype(T, :metadata_type))

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

# TODO doc check_metadata
"""
    check_metadata(style::MetadataStyle, context, metadata)
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
function check_metadata(@nospecialize(s::NamedStyle), @nospecialize(ctx), @nospecialize(md::NamedTuple))
    _check(ctx, md)
end
function _check(ctx, md::NamedTuple)
    for i in 1:nfields(md)
        md_i = getfield(md, i)
        check_metadata(combine_styles(md_i), ctx, md_i)
    end
    nothing
end


# TODO document propagate_metadata(::MetadataStyle, md)
"""
    propagate_metadata(style::MetadataStyle, md)
"""
propagate_metadata(::PersistentStyle, @nospecialize(md)) = md
propagate_metadata(::MetadataStyleStyle, @nospecialize(md)) = md
propagate_metadata(::DefaultStyle, @nospecialize(md)) = undefvalue
function propagate_metadata(s::NamedStyle, mdn::MetadataNode)
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
permutedims_metadata(s::MetadataStyle, @nospecialize(md), perm::Tuple) = propagate_metadata(s, md)

# TODO doc combine_metadata()
"""
    combine_metadata(style::MetadataStyle, mds::Tuple)
"""
combine_metadata(::DefaultStyle, @nospecialize(mds::Tuple)) = undefvalue
combine_metadata(::MetadataStyleStyle, ::Tuple{}) = DefaultStyle()
combine_metadata(s::MetadataStyleStyle, mds::Tuple{Any,Vararg{Any}}) = combine_metadata(s, Base.tail(mds))
@inline function combine_metadata(s::MetadataStyleStyle, mds::Tuple{MetadataStyle,Vararg{Any}})
    style_rule(getfield(mds, 1), combine_metadata(s, Base.tail(mds)))
end
function combine_metadata(style::NamedStyle, @nospecialize(mds::Tuple))
    combine(combine_metadata, mds)
end
# use the same principles as merge here, take the leftmost existing value
combine_metadata(::PersistentStyle, ::Tuple{}) = undefvalue
function combine_metadata(::PersistentStyle, ns::Tuple{Any,Vararg{Any}})
    n = getfield(ns, 1)
    isundefnode(n) ? combine_metadata(PersistentStyle(), Base.tail(ns)) : n
end
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
combine(f::Function, ctxs::Tuple) = _combine(f, style_rule(map(trynames, ctxs)...), ctxs)
function _combine(f::Function, ::NamedStyle{syms}, ctxs::Tuple) where {syms}
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
