
@nospecialize

# TODO doc
struct NoMetadata <: AbstractDict{Symbol,Union{}} end
Base.keys(::NoMetadata) = ()
Base.values(::NoMetadata) = ()
Base.haskey(::NoMetadata, key) = false
Base.get(::NoMetadata, key, d) = d
Base.get(f::Union{Type,Function}, ::NoMetadata, key) = f()
Base.iterate(::NoMetadata) = nothing
Base.in(key, ::NoMetadata) = false

abstract type MetadataStyle end

struct MetadataPersistent <: MetadataStyle end

struct MetadataDefault <: MetadataStyle end

struct MetadataUnknown <: MetadataStyle end

# TODO document
"""
    MetadataStyle
"""
MetadataStyle(x::MetadataStyle) = x
MetadataStyle(x) = MetadataStyle(typeof(x))
MetadataStyle(T::Type) = MetadataDefault()

# TODO
check_metadata(data, md) = nothing

@specialize
