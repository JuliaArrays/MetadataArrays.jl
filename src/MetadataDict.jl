
struct MetadataDict{K, V, P <: Union{AbstractDict{K, V}, NamedTuple{<:Any, <:Tuple{Vararg{V}}}}, M <: MDType, S<:MetadataStyle} <: AbstractDict{K, V}
    parent::P
    metadata::M
    style::S

    function MetadataDict{K, V, P, M, S}(p::Union{AbstractDict, NamedTuple}=D(), m=M(), s=S()) where {K, V, P, M, S}
        new{K, V, P, M, S}(p, m, s)
    end
    function MetadataDict{K, V, P, M}(p::Union{AbstractDict, NamedTuple}=P(), m=NamedTuple(), s=MetadataStyle()) where {K, V, P, M}
        MetadataDict{K, V, P, M, typeof(s)}(p, m, s)
    end
    function MetadataDict{K, V, P}(p::Union{AbstractDict, NamedTuple}=P(), m=NamedTuple(), s=MetadataStyle()) where {K, V, P}
        MetadataDict{K, V, P, typeof(m)}(p, m, s)
    end
    function MetadataDict{K, V}(d::Union{AbstractDict, NamedTuple}, m=NamedTuple(), s=MetadataStyle()) where {K, V}
        MetadataDict{K, V, typeof(d)}(d, m, s)
    end
    function MetadataDict{K}(d::AbstractDict, m=NamedTuple(), s=MetadataStyle()) where {K}
        MetadataDict{K, valtype(d)}(d, m, s)
    end
    function MetadataDict(d::AbstractDict, m::MDType=NamedTuple(), s=MetadataStyle())
        MetadataDict{keytype(d)}(d, m, s)
    end

    # NamedTuple support
    function MetadataDict{Symbol}(p::NamedTuple, m::MDType=NamedTuple(), s=MetadataStyle())
        MetadataDict{Symbol, eltype(p)}(p, m, s)
    end
    function MetadataDict(p::NamedTuple, m::MDType=NamedTuple(), s=MetadataStyle())
        MetadataDict{Symbol}(p, m, s)
    end

    function Base.copy(mdd::MetadataDict{K, V, P, M}) where {K, V, P, M}
        p = copy(getfield(mdd, :parent))
        if (M <: NamedTuple)
            m = getfield(mdd, :metadata)
        else
            m = copy(getfield(mdd, :metadata))
        end
        s = getfield(mdd, :style)
        new{K, V, P, M, typeof(s)}(p, m, s)
    end
end

const NamedMetadataDict{K, V, P, MDNS, MDTYS} = MetadataDict{K, V, P, NamedTuple{MDNS, MDTYS}}

Base.parent(mdd::MetadataDict) = getfield(mdd, :parent)
ArrayInterface.parent_type(@nospecialize(T::Type{<:MetadataDict})) = fieldtype(T, :parent)

Base.propertynames(mda::MetadataDict) = propertynames(getfield(mda, :parent))
Base.hasproperty(mda::MetadataDict, s::Symbol) = hasproperty(getfield(mda, :parent), s)
Base.getproperty(mda::MetadataDict, s::Symbol) = getproperty(getfield(mda, :parent), s)
function Base.setproperty!(mda::MetadataDict, s::Symbol, v)
    setproperty!(getfield(mda, :parent), s, v)
end

function ArrayInterface.can_setindex(@nospecialize(T::Type{<:MetadataDict}))
    can_setindex(fieldtype(T, :parent))
end

function ArrayInterface.can_change_size(@nospecialize(T::Type{<:MetadataDict}))
    can_change_size(fieldtype(T, :parent))
end

ArrayInterface.is_forwarding_wrapper(@nospecialize(T::Type{<:MetadataDict})) = true

function Base.sizehint!(mdd::MetadataDict, n::Integer)
    sizehint!(getfield(mdd, :parent), n)
    return mdd
end
function Base.push!(mdd::MetadataDict, p::Pair)
    push!(getfield(mdd, :parent), p)
    return mdd
end
function Base.push!(mdd::MetadataDict, p::Pair, ps::Pair...)
    push!(getfield(mdd, :parent), p, ps...)
    return mdd
end

Base.pop!(mdd::MetadataDict, k) = pop!(getfield(mdd, :parent), k)
Base.pop!(mdd::MetadataDict, k, d) = pop!(getfield(mdd, :parent), k, d)
function Base.empty!(mdd::MetadataDict)
    empty!(getfield(mdd, :parent))
    return mdd
end
function Base.delete!(mdd::MetadataDict, key)
    delete!(getfield(mdd, :parent), key)
    return mdd
end

Base.get(mdd::MetadataDict, key, default) = get(getfield(mdd, :parent), key, default)
function Base.get(f::Union{Type,Function}, mdd::MetadataDict, key)
    get(f, getfield(mdd, :parent), key)
end
function Base.get!(mdd::MetadataDict, key, default)
    get!(getfield(mdd, :parent), key, default)
end
function Base.get!(f::Union{Type,Function}, mdd::MetadataDict, key)
    get!(f, getfield(mdd, :parent), key)
end

Base.@propagate_inbounds Base.getindex(mdd::MetadataDict, k) = getfield(mdd, :parent)[k]
Base.@propagate_inbounds function Base.setindex!(mdd::MetadataDict, v, k)
    setindex!(getfield(mdd, :parent), v, k)
end

Base.haskey(mdd::MetadataDict, k) = haskey(getfield(mdd, :parent), k)

Base.iterate(mdd::MetadataDict, args...) = iterate(getfield(mdd, :parent), args...)

Base.length(mdd::MetadataDict) = length(getfield(mdd, :parent))
Base.first(mdd::MetadataDict) = first(getfield(mdd, :parent))
Base.last(mdd::MetadataDict) = last(getfield(mdd, :parent))
Base.isempty(mdd::MetadataDict) = isempty(getfield(mdd, :parent))
Base.keys(mdd::MetadataDict) = keys(getfield(mdd, :parent))
Base.values(mdd::MetadataDict) = values(getfield(mdd, :parent))
#endregion AbstractDict Interface

## merge and mergewith
# fall back to Symbol if we don't clearly have String
_promote_keytypes(@nospecialize(pds::Tuple{Vararg{MetadataDict{String}}})) = String
_promote_keytypes(@nospecialize(pds::Tuple{Vararg{MetadataDict}})) = Symbol
_promote_valtypes(V) = V
function _promote_valtypes(V, d, ds...)  # give up if promoted to any
    if V === Any
        return Any
    else
        return _promote_valtypes(promote_type(V, valtype(d)), ds...)
    end
end

Base.merge(pd::MetadataDict) = copy(pd)
Base.merge(pd::NamedMetadataDict, pds::NamedMetadataDict...) = _mergeprops(_getarg2, pd, pds...)
_getarg2(@nospecialize(arg1), @nospecialize(arg2)) = arg2
function Base.merge(pd::MetadataDict, pds::MetadataDict...)
    K = _promote_keytypes((pd, pds...))
    V = _promote_valtypes(valtype(pd), pds...)
    out = MetadataDict(Dict{K,V}())
    for (k,v) in pd
        out[k] = v
    end
    merge!(out, pds...)
end

Base.mergewith(combine, pd::MetadataDict) = copy(pd)
function Base.mergewith(combine, pd::MetadataDict, pds::MetadataDict...)
    K = _promote_keytypes((pd, pds...))
    V0 = _promote_valtypes(valtype(pd), pds...)
    V = promote_type(Core.Compiler.return_type(combine, Tuple{V0,V0}), V0)
    out = MetadataDict(Dict{K,V}())
    for (k,v) in pd
        out[k] = v
    end
    mergewith!(combine, out, pds...)
end
@inline function Base.mergewith(combine, pd::NamedMetadataDict, pds::NamedMetadataDict...)
    _mergeprops(combine, pd, pds...)
end
_mergeprops(combine, @nospecialize(x::NamedMetadataDict)) = x
@inline function _mergeprops(combine, x::NamedMetadataDict, y::NamedMetadataDict)
    MetadataDict(mergewith(combine, getfield(x, :data), getfield(y, :data)))
end
@inline function _mergeprops(combine, x::NamedMetadataDict, y::NamedMetadataDict, zs::NamedMetadataDict...)
    _mergeprops(combine, _mergeprops(combine, x, y), zs...)
end

metadata(mdd::MetadataDict) = getfield(mdd, :metadata)
function metadata(mdd::MetadataDict, key; style::Bool=false)
    md = getfield(mdd, :metadata)[key]
    if style
        return md
    else
        return getfield(md, 1)
    end
end
function metadata(mdd::MetadataDict, key, default; style::Bool=false)
    md = get(getfield(mdd, :metadata), key, NamedTuple())
    if md === NamedTuple()
        return default
    elseif style
        return md
    else
        return getfield(md, 1)
    end
    metadata(x, Symbol(key), default; style=style)
end
function metadata!(mdd::MetadataDict, key, value; style=:default)
    metadatasupport(typeof(mdd)).write || throw(MethodError(deletemetadata!, (mdd, key)))
    setindex!(getfield(mdd, :metadata), key, (value, style))
    return mdd
end
function deletemetadata!(mdd::MetadataDict, key)
    metadatasupport(typeof(mdd)).write || throw(MethodError(deletemetadata!, (mdd, key)))
    delete!(getfield(mdd, :metadata), key)
    return mdd
end
function emptymetadata!(mdd::MetadataDict)
    metadatasupport(typeof(mdd)).write || throw(MethodError(deletemetadata!, (mdd, key)))
    empty!(getfield(mdd, :metadata))
    return mdd
end

metadatakeys(mdd::MetadataDict) = keys(getfield(mdd, :metadata))
function metadatasupport(T::Type{<:MetadataDict})
    (read=true, write=ArrayInterface.can_setindex(fieldtype(T, :metadata)))
end

