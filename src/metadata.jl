# This file defines the metadata interface

#region self metadata
function metadatasupport(T::Type{<:MetadataArray})
    (read=true, write=ArrayInterface.can_setindex(fieldtype(T, :metadata)))
end

metadatakeys(mda::MetadataArray) = keys(getfield(mda, :metadata))

metadata(mda::MetadataArray) = getfield(mda, :metadata)
@inline function metadata(mda::MetadataArray, key::AbstractString; style::Bool=false)
    if style
        return (getfield(mda, :metadata)[key], :default)
    else
        return getfield(mda, :metadata)[key]
    end
end
# TODO Is `default` of type `(value, style)`
@inline function metadata(mda::MetadataArray, key::AbstractString, default; style::Bool=false)
    if style
        return (get(getfield(mda, :metadata), key, default), :default)
    else
        return get(getfield(mda, :metadata), key, default)
    end
end
@inline function metadata(mda::MetadataArray, key::Symbol; style::Bool=false)
    if style
        return (getfield(mda, :metadata)[key], :default)
    else
        return getfield(mda, :metadata)[key]
    end
end
@inline function metadata(mda::MetadataArray, key::Symbol, default; style::Bool=false)
    if style
        return (get(getfield(mda, :metadata), key, default), :default)
    else
        return get(getfield(mda, :metadata), key, default)
    end
end

@inline function metadata!(mda::MetadataArray, key::Symbol, val; style=:default)
    setindex!(getfield(mda, :metadata), (val, style), key)
end

function deletemetadata!(mda::MetadataArray, key)
    metadatasupport(typeof(mda)).write || throw(MethodError(emptymetadata!, (mda, key)))
    delete!(getfield(mda, :metadata), key)
end

function emptymetadata!(mda::MetadataArray)
    metadatasupport(typeof(mda)).write || throw(MethodError(emptymetadata!, (mda,)))
    empty!(getfield(mda, :metadata))
end
#endregion self metadata

dropmeta(mda::MetadataArray) = getfield(mda, :parent)
dropmeta(x) = x


# selfmeta(mda::MetadataArray) = getfield(mda, :metadata)
# selfmeta(x) = UNDEF_META

# dimsmeta(mda::MetadataArray) = getfield(mda, :dimensions)
# dimsmeta(x) = ntuple(Returns(UNDEF_DIMS_META), Val{ndims(x)}())
