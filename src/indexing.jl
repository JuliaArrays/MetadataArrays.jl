
Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, i::Int...)
    getfield(mda, :parent)[i...]
end
Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, inds...)
    MetadataArray(getfield(mda, :parent)[inds...], getfield(mda, :metadata))
end
Base.@propagate_inbounds function Base.view(mda::MetadataArray, inds...)
    MetadataArray(view(getfield(mda, :parent), inds...), getfield(mda, :metadata))
end
Base.@propagate_inbounds function Base.setindex!(mda::MetadataArray, val, inds...)
    setindex!(getfield(mda, :parent),val, inds...)
end
