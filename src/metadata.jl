# This file defines the metadata interface

#region self metadata
function metadatasupport(T::Type{<:MetadataArray})
    (read=true, write=ArrayInterface.can_setindex(fieldtype(T, :metadata)))
end
function metadatasupport(T::Type{<:MetadataUnitRange})
    (read=true, write=ArrayInterface.can_setindex(fieldtype(T, :metadata)))
end

metadatakeys(mda::Union{MetadataArray, MetadataUnitRange}) = keys(getfield(mda, :metadata))
@inline function metadata(mda::Union{MetadataArray, MetadataUnitRange}, key; style::Bool=false)
    if style
        return (getfield(mda, :metadata)[key], :default)
    else
        return getfield(mda, :metadata)[key]
    end
end
@inline function metadata(mda::Union{MetadataArray, MetadataUnitRange}, key, default; style::Bool=false)
    if style
        return (get(getfield(mda, :metadata), key, default), :default)
    else
        return get(getfield(mda, :metadata), key, default)
    end
end
@inline function metadata!(mda::Union{MetadataArray, MetadataUnitRange}, key, val; style=:default)
    setindex!(getfield(mda, :metadata), (val, style), key)
end

function deletemetadata!(mda::Union{MetadataArray, MetadataUnitRange}, key)
    metadatasupport(typeof(mda)).write || throw(MethodError(emptymetadata!, (mda, key)))
    delete!(getfield(mda, :metadata), key)
end

function emptymetadata!(mda::Union{MetadataArray, MetadataUnitRange})
    metadatasupport(typeof(mda)).write || throw(MethodError(emptymetadata!, (mda,)))
    empty!(getfield(mda, :metadata))
end
#endregion self metadata

dropmeta(mda::Union{MetadataArray, MetadataUnitRange}) = getfield(mda, :parent)
dropmeta(x) = x

# FIXME: permute_dimsmetadata
function permute_dimsmetadata(mda::MetadataArray, dims)
    getfield(mda, :metadata)
end
function permute_dimsmetadata(mda::MetadataArray)
    getfield(mda, :metadata)
end

# single dimension
# select_dimmeta(a::AbstractArray, dim::Int) = UNDEF_DIMS_META
# function select_dimmeta(mda::MetadataArray, dim::Int)
#     if dim > ndims(mda)
#         return UNDEF_DIMS_META
#     else
#         return getfield(getfield(mda, :dimensions), dim)
#     end
# end

# FIXME: select_dimsmetadata
function select_dimsmetadata(mda::MetadataArray, dim::Int) end

# FIXME: reduce_dimsmetadata doesn't account for dimension-wsie metadata yet
function reduce_dimsmetadata(mda::MetadataArray, dim::Tuple)
    getfield(mda, :metadata)
end
reduce_dimsmetadata(mda::MetadataArray, dim::Int) = reduce_dimsmetadata(mda, (dim,))
function reduce_dimsmetadata(mda::MetadataArray, ::Colon)
    reduce_dimsmetadata(mda, ntuple(+, Val(ndims(mda))))
end
function reduce_dimsmetadata(a::AbstractArray, dim::Int)
    reduce_dimsmetadata(dimsmetadata(a, dim))
end
