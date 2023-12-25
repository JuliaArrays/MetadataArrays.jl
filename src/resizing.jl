
# TODO:
# function Base.append!(v::MetadataVector, iters...)
#     ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(append!, (v, iters...)))
#     append!(getfield(v, :parent), iters...)
#     for (key, val) in get_indices_metadata_dict(v, 1)
#         append!(val, try_metadata_indices.(iters, key)...)
#     end
#     return v
# end

# TODO get_indices_metadata_dict(mda::MetadataArray, dim::Int)

# function ArrayInterface.can_change_size(T::Type{<:ArrayMetadata})
#     D = fieldtype(T, :dimensions)
#     fieldcount(D) === 1 && can_change_size(fieldtype(D, 1))
# end

# TODO: fix this so it only checks metadata for first dim indices in
function Base.sizehint!(mda::MetadataVector, n::Integer)
    sizehint!(parent(mda), n)
end
function Base.resize!(v::MetadataVector, n::Integer)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(resize!, (v, n)))
    resize!(getfield(v, :parent), n)
    for (key, val) in get_indices_metadata_dict(v, 1)
        resize!(val, n)
    end
    return v
end
function Base.insert!(v::MetadataVector, i::Integer, item)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(insert!, (v, i, item)))
    insert!(getfield(v, :parent), i, item)
    for (key, val) in get_indices_metadata_dict(v, 1)
        # insert!(val, i, metadata(item, key))
        insert!(val, i, undefmeta)
    end
    return v
end

function Base.push!(v::MetadataVector, item)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(push!, (v, item)))
    push!(getfield(v, :parent), item)
    for (key, val) in get_indices_metadata_dict(v, 1)
        # push!(val, metadata(item, key))
        push!(val, undefmeta)
    end
    return v
end

function Base.pushfirst!(v::MetadataVector, item)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(pushfirst!, (v, item)))
    pushfirst!(getfield(v, :parent), item)
    for (key, val) in get_indices_metadata_dict(v, 1)
        # pushfirst!(val, metadata(item, key))
        push!(val, undefmeta)
    end
    return v
end

function Base.append!(v::MetadataVector, iters...)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(append!, (v, iters...)))
    append!(getfield(v, :parent), iters...)
    new_size = sum(length, iters) + length(v)
    for (key, val) in get_indices_metadata_dict(v, 1)
        resize!(val, new_size)
    end
    return v
end
function Base.prepend!(v::MetadataVector, iters...)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(prepend!, (v, iters...)))
    prepend!(getfield(v, :parent), iters...)
    new_size = sum(length, iters) + length(v)
    for (key, val) in get_indices_metadata_dict(v, 1)
        resize!(val, new_size)
    end
    return v
end

function Base.deleteat!(v::MetadataVector, inds)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(deleteat!, (v, inds)))
    deleteat!(getfield(v, :parent), inds)
    for (key, val) in get_indices_metadata_dict(v, 1)
        deleteat!(val, inds)
    end
    return v
end
function Base.keepat!(v::MetadataVector, inds)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(keepat!, (v, inds)))
    keepat!(getfield(v, :parent), inds)
    for (key, val) in get_indices_metadata_dict(v, 1)
        keepat!(val, inds)
    end
    return v
end
function Base.empty!(v::MetadataVector)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(empty!, (v,)))
    empty!(getfield(v, :parent))
    for (key, val) in get_indices_metadata_dict(v, 1)
        empty!(val)
    end
    return v
end
function Base.pop!(v::MetadataVector)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(pop!, (v,)))
    item = pop!(getfield(v, :parent))
    for (key, val) in get_indices_metadata_dict(v, 1)
        pop!(val)
    end
    return item
end

function Base.popfirst!(v::MetadataVector)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(popfirst!, (v,)))
    item = popfirst!(getfield(v, :parent))
    for (key, val) in get_indices_metadata_dict(v, 1)
        popfirst!(val)
    end
    return item
end

function Base.popat!(v::MetadataVector, i::Integer)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(popat!, (v, i)))
    item = popat!(getfield(v, :parent), i)
    for (key, val) in get_indices_metadata_dict(v, 1)
        popat!(val, i)
    end
    return item
end
function Base.popat!(v::MetadataVector, i::Integer, default)
    ArrayInterface.can_change_size(typeof(v)) || throw(MethodError(popat!, (v, i, default)))
    item = popat!(getfield(v, :parent), i, default)
    for (key, val) in get_indices_metadata_dict(v, 1)
        popat!(val, i, default)
    end
    return item
end
#endregion
