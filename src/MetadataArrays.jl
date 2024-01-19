module MetadataArrays

using ArrayInterface
import ArrayInterface: parent_type, is_forwarding_wrapper, can_setindex, can_change_size
using Base: BroadcastStyle
using DataAPI
import DataAPI: metadata, metadata!, metadatakeys, metadatasupport, deletemetadata!,
    emptymetadata!
using LinearAlgebra
using Statistics

export
    MetadataArray,
    MetadataMatrix,
    MetadataVector,
    deletemetadata!,
    emptymetadata!,
    metadata,
    metadata!,
    metadatakeys

const MDType = Union{NamedTuple, AbstractDict{Symbol}, AbstractDict{String}}

include("MetadataDicts.jl")
include("types.jl")
include("array.jl")
include("metadata.jl")
include("reduce.jl")

Base.write(io::IO, mda::MetadataArray) = write(io, getfield(mda, :parent))
function Base.read!(io::IO, mda::MetadataArray)
    read!(io, getfield(mda, :parent))
    return mda
end
function Base.reshape(s::MetadataArray, d::Dims)
    MetadataArray(reshape(parent(s), d), getfield(mda, :metadata))
end

include("indexing.jl")
include("resizing.jl")
include("broadcasting.jl")

end # module

