
@assume_effects :total function _find_defined(t::Tuple{Vararg{Any}})
    @nospecialize t
    out = Int[]
    for i in 1:nfields(t)
        if getfield(t, i) !== undefvalue
            push!(out, i)
        end
    end
    (out...,)
end
@assume_effects :total function _findsym_tuple(s::Symbol, syms::Tuple{Vararg{Symbol}})
    @nospecialize syms
    for i in 1:nfields(syms)
        getfield(syms, i) === s && return i
    end
    return 0
end
@assume_effects :total function _merge_names(nms::Tuple{Vararg{Tuple{Vararg{Symbol}}}})
    N = nfields(nms)
    if N === 0
        return ()
    elseif N === 1
        return getfield(nms, 1)
    else
        names = Symbol[getfield(nms, 1)...]
        for i in 2:N
            for name in getfield(nms, i)
                if _findsym_tuple(n, an) === 0
                    push!(names, n)
                end
            end
        end
        return (names...,)
    end
end

@nospecialize

"""
    MetadataNode{S<:MetadataStyle}(data[, context])

Dedicated node type for associating the style `S` with `data`. `context` may be provided,
describing the context in which `data` becomes metadata.
"""
struct MetadataNode{S<:MetadataStyle,M,C}
    metadata::M
    context::C

    MetadataNode{S}(data, ctx) where {S} = new{S,typeof(data),typeof(ctx)}(data, ctx)
    MetadataNode{S}(data) where {S} = MetadataNode{S}(data, undefvalue)
    MetadataNode(data, ctx) = MetadataNode{typeof(MetadataStyle(data))}(data, ctx)
    MetadataNode(data) = MetadataNode{typeof(MetadataStyle(data))}(data)
end

metadatasupport(T::Type{<:MetadataNode}) = (read=true, write=false)
metadata(mdn::MetadataNode) = getfield(mdn, :metadata)
context(mdn::MetadataNode) = getfield(mdn, :context)

MetadataStyle(::Union{Type{<:MetadataNode{S}},MetadataNode{S}}) where {S} = S()

Base.dataids(mdn::MetadataNode) = Base.dataids(metadata(mdn))

# TODO manage conversions where `UndefValue` drops fields
function Base.convert(T::Type{<:MetadataNode}, mdn::MetadataNode)
    if isa(mdn, T)
        mdn
    else
        MetadataNode{typeof(MetadataStyle(T))}(
            convert(fieldtype(T, :metadata), metadata(mdn)),
            convert(fieldtype(T, :context), context(mdn))
        )
    end
end

metadatakeys(mdn::MetadataNode) = ()
metadatakeys(mdn::MetadataNode{MetadataNamed,NamedTuple{syms}}) where {syms} = syms
metadata(mdn::MetadataNode{MetadataNamed}, s::Symbol) = getfield(metadata(mdn), s)
metadata(mdn::MetadataNode, ::Symbol, d) = d
metadata(mdn::MetadataNode{MetadataNamed}, key::Symbol, d) = get(metadata(mdn), key, d)

Base.haskey(mdn::MetadataNode{MetadataNamed}, s::Symbol) = haskey(metadata(mdn), s)
Base.isempty(mdn::MetadataNode{MetadataNamed,NamedTuple{(),Tuple{}}}) = true
Base.isempty(mdn::MetadataNode{MetadataNamed}) = false

function Base.show(io::IO, m::MIME"text/plain", mdn::MetadataNode)
    print(io, "MetadataNode{$(nameof(typeof(MetadataStyle(mdn))))}")
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
isundefnode(::MetadataNode{MetadataDefault,UndefValue}) = true

"""
    delete_metadata(data)

Returns an instance of `data` without any attached metadata, without mutating any data.
If `data` has no metadata attached, then `data` is returned unchanged.
"""
delete_metadata(data) = data
delete_metadata(mdn::MetadataNode) = MetadataNode(undefvalue, context(mdn))

@specialize

# TODO doc check_metadata
"""
    check_metadata([style::MetadataStyle], context, metadata)
"""
function check_metadata(@nospecialize(ctx), @nospecialize(md))
    check_metadata(MetadataStyle(md), ctx, md)
end
check_metadata(::MetadataPersistent, @nospecialize(ctx), @nospecialize(md)) = nothing
check_metadata(::MetadataDefault, @nospecialize(ctx), @nospecialize(md)) = nothing
function check_metadata(::MetadataNamed, @nospecialize(ctx), @nospecialize(mdn::MetadataNode))
    check_metadata(MetadataNamed(), ctx, metadata(mdn))
end
check_metadata(::MetadataNamed, @nospecialize(ctx), @nospecialize(md::NamedTuple)) = _check(ctx, md)

# TODO document propagate_metadata(::MetadataStyle, md)
"""
    propagate_metadata([style::MetadataStyle], md)
"""
propagate_metadata(@nospecialize(md)) = propagate_metadata(MetadataStyle(md), md)
propagate_metadata(::MetadataPersistent, @nospecialize(md)) = md
propagate_metadata(::MetadataDefault, @nospecialize(md)) = undefvalue
function propagate_metadata(::MetadataNamed, mdn::MetadataNode)
    MetadataNode{MetadataNamed}(propagate_metadata(MetadataNamed(), metadata(mdn)))
end
function propagate_metadata(::MetadataNamed, @nospecialize(md::NamedTuple))
    _propagate(propagate_metadata, md)
end

"""
    permutedims_metadata(style::MetadataStyle, md[, perm::Tuple])

Provides a hook for managing metadata aligned to dimensions between dimension permuting
methods (`adjoint`, `transpose`, `permutedims`). If this method isn't overloaded for
the given `style`, `propagate_metadata(style, md)` is called.
"""
permutedims_metadata(s::MetadataStyle, @nospecialize(md)) = propagate_metadata(s, md)
permutedims_metadata(@nospecialize(md)) = permutedims_metadata(MetadataStyle(md), md)
function permutedims_metadata(@nospecialize(md), perm::Tuple)
    permutedims_metadata(MetadataStyle(md), md, perm)
end
function permutedims_metadata(s::MetadataStyle, @nospecialize(md), perm::Tuple)
    propagate_metadata(s, md)
end

# TODO doc combine_metadata()
"""
    combine_metadata([style::MetadataStyle], nodes::Tuple)
"""
combine_metadata(@nospecialize(ns::Tuple)) = combine_metadata(MetadataStyle(ns...), ns)
combine_metadata(::MetadataDefault, @nospecialize(ns::Tuple)) = undefvalue
combine_metadata(::MetadataNamed, @nospecialize(ns::Tuple)) = _combine(combine_metadata, ns)
# use the same principles as merge here, take the leftmost existing value
combine_metadata(::MetadataPersistent, ::Tuple{}) = undefvalue
function combine_metadata(::MetadataPersistent, ns::Tuple{Any,Vararg{Any}})
    n = getfield(ns, 1)
    isundefnode(n) ? combine_metadata(MetadataPersistent(), Base.tail(ns)) : n
end
function _check(ctx, md::NamedTuple)
    for i in 1:nfields(md)
        md_i = getfield(md, i)
        check_metadata(MetadataStyle(md_i), ctx, md_i)
    end
    nothing
end

@inline function _propagate(f::Function, nt::NamedTuple{syms}) where {syms}
    t = ntuple(i-> f(getfield(nt, i)), Val{nfields(nt)}())
    inds = _find_defined(t)
    if inds === ()
        return undefvalue
    else
        new_keys = ntuple(i -> getfield(syms, getfield(inds, i)), Val{nfields(inds)}())
        new_vals = ntuple(i -> getfield(t, getfield(inds, i)), Val{nfields(inds)}())
        return NamedTuple{new_keys}(new_vals)
    end
end

@inline function _combine(f::Function, ctxs::Tuple)
    vn = Val{nfields(ctxs)}()
    syms = _merge_names(map(_getmetakeys, ctxs))
    t = ntuple(Val{nfields(syms)}()) do i
        key_i = getfield(syms, i)
        f(ntuple(vn) do j
            ctx_j = getfield(ctxs, j)
            data = _getmeta(ctx_j, key_i, undefvalue)
            data === undefvalue ? MetadataNode(data, ctx_j) : data
        end)
    end
    inds = _find_defined(t)
    if inds === ()
        return undefvalue
    else
        ninds = Val{nfields(inds)}()
        new_keys = ntuple(i -> getfield(syms, getfield(inds, i)), ninds)
        new_vals = ntuple(i -> getfield(t, getfield(inds, i)), ninds)
        return NamedTuple{new_keys}(new_vals)
    end
end

