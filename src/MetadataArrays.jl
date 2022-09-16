module MetadataArrays

export MetadataArray, MetadataVector, metadata

struct MetadataNotAvailable end
const mdna = MetadataNotAvailable()

include("metadataarray.jl")

# metadata_type(data)
metadata_type(data) = metadata_type(typeof(data))
metadata_type(@nospecialize T::Type{<:MetadataArray}) = fieldtype(T, :metadata)

# is_symbol_metadata_keytype
function is_symbol_metadata_keytype(@nospecialize data)
    !(metadata_type(data) <: AbstractDict{String})
end

# is_property_metadata
is_property_metadata(@nospecialize data) = !(metadata_type(data) <: AbstractDict)

# to_metadata_keytype
@inline function to_metakeytype(@nospecialize(data), key::Symbol)
    is_symbol_metadata_keytype(data) ? key : String(key)
end
@inline function to_metakeytype(@nospecialize(data), key::AbstractString)
    is_symbol_metadata_keytype(data) ? Symbol(key) : key
end

# metakeys(data)
@inline function metakeys(data)
    is_property_metadata(data) ? propertynames(metadata(data)) : keys(metadata(data))
end

# hasmetakey(data, key)
@inline hasmetakey(data, key) = to_metakeytype(data, key) in metakeys(data)

# metadata(data, key)
function metadata(data, key::Symbol)
    md = metadata(data)
    if md isa AbstractDict
       return md[keytype(md) <: Symbol ? key : String(key)]
    else
        return getproperty(md, key)
    end
end
function metadata(data, key::AbstractString)
    md = metadata(data)
    if md isa AbstractDict
        return md[keytype(md) <: Symbol ? Symbol(key) : key]
    else
        return getproperty(md, Symbol(key))
    end
end

# metadata!(data, key)
function metadata!(data, key::Symbol, v)
    md = metadata(data)
    if md isa AbstractDict
        return setindex!(md, v, keytype(md) <: Symbol ? key : String(key))
    else
        return setproperty!(md, key, v)
    end
end
function metadata!(data, key::AbstractString, v)
    md = metadata(data)
    if md isa AbstractDict
        return setindex!(md, v, keytype(md) <: Symbol ? Symbol(key) : key)
    else
        return setproperty!(md, Symbol(key), v)
    end
end


# getmeta
function getmeta(data, key::Symbol, default)
    md = metadata(data)
    if md isa AbstractDict
        return get(md, keytype(md) <: Symbol ? key : String(key), default)
    else
        return in(key, propertynames(md)) ? getproperty(md, key) : default
    end
end
function getmeta(data, key::AbstractString, default)
    md = metadata(data)
    if md isa AbstractDict
        return get(md, keytype(md) <: Symbol ? Symbol(key) : key, default)
    else
        key2 = Symbol(key)
        return in(key2, propertynames(md)) ? getproperty(md, key2) : default
    end
end

# _merge_propertynames
_merge_propertynames(x::Tuple{Vararg{Symbol}}, y::Tuple{Symbol,Vararg{Symbol}}) = (x..., y...)
_merge_propertynames(@nospecialize(x), ::Tuple{}) = x
_merge_propertynames(@nospecialize(x), @nospecialize(y)) = [x..., y...]

end # module
