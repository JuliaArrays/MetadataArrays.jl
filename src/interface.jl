
@assume_effects :total function find_data(t::Tuple{Vararg{Any}})
    @nospecialize t
    out = Int[]
    for i in 1:nfields(t)
        if getfield(t, i) !== NoData()
            push!(out, i)
        end
    end
    (out...,)
end

@nospecialize

"""
    NoMetadata

Internal type for the `Metadata` package that indicates the absence of any metadata.
_DO NOT_ store metadata with the value `NoMetadata()`.

!!! warning
    This is not part of the public API and may change without notice.
"""
struct NoMetadata <: AbstractDict{Symbol,Union{}} end

Base.keys(::NoMetadata) = ()
Base.values(::NoMetadata) = ()
Base.haskey(::NoMetadata, key) = false
Base.get(::NoMetadata, key, d) = d
Base.iterate(::NoMetadata) = nothing
Base.in(key, ::NoMetadata) = false

abstract type MetadataStyle end

"""
    MetadataPersistent

Subtype of `MetadataStyle` indicated that its associated metadata should persist
across operations that copy its contextual data.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataPersistent <: MetadataStyle end

"""
    MetadataDefault

The default type returned by `MetadataStyle(type)`.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataDefault <: MetadataStyle end

"""
    MetadataNamed

Subtype of `MetadataStyle` indicating that it's associated data is a `NamedTuple` where
each field is also metadata.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataNamed <: MetadataStyle end


# TODO doc-MetadataNode
"""
    MetadataNode{S<:MetadataStyle}(data)
"""
struct MetadataNode{S<:MetadataStyle,D}
    data::D

    MetadataNode{S}(data) where {S} = new{S,typeof(data)}(data)
    MetadataNode(data) = MetadataNode{typeof(MetadataStyle(data))}(data)
end

_data(mdn::MetadataNode) = getfield(mdn, :data)

Base.dataids(mdn::MetadataNode) = Base.dataids(_data(mdn))

function Base.convert(T::Type{<:MetadataNode}, mdn::MetadataNode)
    if isa(mdn, T)
        mdn
    else
        MetadataNode{typeof(MetadataStyle(T))}(convert(fieldtype(T, :data), _data(mdn)))
    end
end

Base.keys(mdn::MetadataNode{MetadataNamed}) = keys(_data(mdn))
Base.getindex(mdn::MetadataNode{MetadataNamed}, s::Symbol) = getfield(_data(mdn), s)
Base.get(mdn::MetadataNode{MetadataNamed}, key::Symbol, d) = get(_data(mdn), key, d)
Base.haskey(mdn::MetadataNode{MetadataNamed}, s::Symbol) = haskey(_data(mdn), s)
Base.values(mdn::MetadataNode{MetadataNamed}) = values(_data(mdn))

function Base.show(io::IO, m::MIME"text/plain", mdn::MetadataNode)
    print(io, "MetadataNode{$(nameof(typeof(MetadataStyle(mdn))))}")
    print(io, "(")
    Base.show(io, m, _data(mdn))
    print(io, ")")
end

# TODO doc MetadataStyle
"""
    MetadataStyle
"""
MetadataStyle(x::MetadataStyle) = x
MetadataStyle(x) = MetadataStyle(typeof(x))
MetadataStyle(T::Type) = MetadataDefault()
MetadataStyle(::Union{Type{<:MetadataNode{S}},MetadataNode{S}}) where {S} = S()

# TODO document propagate_metadata(::MetadataStyle, md)
"""
    propagate_metadata([style::MetadataStyle], md)
"""
propagate_metadata(md) = propagate_metadata(MetadataStyle(md), md)
propagate_metadata(::MetadataPersistent, md) = md
propagate_metadata(::MetadataDefault, md) = NoMetadata()
propagate_metadata(::MetadataNamed, md::NamedTuple) = _propagate(propagate_metadata, md)
function propagate_metadata(::MetadataNamed, mdn::MetadataNode)
    MetadataNode{MetadataNamed}(propagate_metadata(MetadataNamed(), _data(mdn)))
end

# TODO doc check_metadata
"""
    check_metadata([style::MetadataStyle}, context, metadata)
"""
check_metadata(ctx, md) = check_metadata(MetadataStyle(md), ctx, md)
check_metadata(::MetadataStyle, ctx, md) = nothing
function check_metadata(::MetadataNamed, ctx, mdn::MetadataNode)
    check_metadata(MetadataNamed(), ctx, _data(mdn))
end
check_metadata(::MetadataNamed, ctx, md::NamedTuple) = _check(ctx, md)

@specialize

function _check(ctx, md::NamedTuple)
    for i in 1:nfields(md)
        md_i = getfield(md, i)
        check_metadata(MetadataStyle(md_i), ctx, md_i)
    end
    nothing
end

# TODO document propagate_metadata(f, ::MetadataCollection, md)
function _propagate(f::Function, nt::NamedTuple{syms}) where {syms}
    t = ntuple(i-> f(getfield(nt, i)), Val{nfields(md)}())
    inds = find_data(t)
    new_keys = ntuple(i -> getfield(syms, getfield(inds, i)), Val{nfields(inds)}())
    new_vals = ntuple(i -> getfield(t, getfield(inds, i)), Val{nfields(inds)}())
    return NamedTuple{new_keys}(new_vals)
end
