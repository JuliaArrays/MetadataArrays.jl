using Aqua
using MetadataArrays
using MetadataArrays:
    MetadataNode,
    MetadataStyle,
    MetadataPersistent,
    propagate_metadata,
    permutedims_metadata
using Test

Aqua.test_all(MetadataArrays)

#=

Pkg.activate(".")
using MetadataArrays
using MetadataArrays: MetadataStyle, propagate_metadata, propagate
using BenchmarkTools

@testset "MetadataVector" begin
    v = [1, 3, 5, 4]
    mdv = MetadataVector(v, "Numbers")
    @test length(mdv) == 4
    @test collect(mdv) == parent(mdv) == copy(mdv)
    @test metadata(mdv) == "Numbers"
    mdv[3] = 50
    @test all(mdv .== [1, 3, 50, 4])
    @test eltype(mdv) == Int
end

@testset "MetadataArray" begin
    a = [1 2; 3 4; 5 4]
    mda = MetadataArray(a, MetadataNode{MetadataPersistent}(Dict(1 => "Not at all", 10 => "A lot")))
    @test size(mda) == size(a)
    @test mda[6] == parent(mda)[6]
    @test collect(mda) == parent(mda) == a == copy(mda)
    @test metadata(mda) == Dict(1 => "Not at all", 10 => "A lot")
    mda[3, 2] = 50
    @test all(mda .== [1 2; 3 4; 5 50])
    @test parent(mda[1:2, 1]) == parent(mda)[1:2, 1]
    @test metadata(mda[1:2, 1]) == metadata(mda)
    @test metadata(rand(2)) === nothing
    @test eltype(mda) == Int

    mda = MetadataArray(rand(4), "test")
    mdar = reshape(mda, 2, 2)
    @test parent(mdar) == reshape(parent(mda), 2, 2)
    @test metadata(mdar) == metadata(mda)
    @test axes(mdar) == (Base.OneTo(2), Base.OneTo(2))
    mda2 = similar(mda, (2, 3))
    @test size(mda2) == (2, 3)
    @test metadata(mda2) == metadata(mda)

    x1 = rand(4, 4)
    m1 = MetadataArray(x1, "something")
    @test IndexStyle(m1) == IndexLinear()
    x2 = view(x1, 1:2, 1:2)
    m2 = MetadataArray(x2, "something")
    @test IndexStyle(m2) == IndexCartesian()
end
=#

struct PersistantAnnotation{A}
    annotation::A
end

MetadataArrays.MetadataStyle(@nospecialize(T::Type{<:PersistantAnnotation})) = MetadataArrays.MetadataPersistent()

a = [1 2; 3 4; 5 4]
md = (m1 =1, annotation=PersistantAnnotation("hello world"));
mda = MetadataArray(a, md);

@test first(mda) == first(a)
@test last(mda) == last(a)
@test size(mda) == size(a)
@test axes(mda) == axes(a)
@test strides(mda) == strides(a)
@test length(mda) == length(a)
@test firstindex(mda) == firstindex(a)
@test lastindex(mda) == lastindex(a)
@test in(1, mda)
@test keys(mda) == keys(a)
@test !isempty(mda)
@test Base.dataids(mda) == Base.dataids(a)
@test IndexStyle(mda) == IndexStyle(a)

@test getproperty(mda, "m1") == 1
@test getproperty(mda, :m1) == 1
@test metadata(mda, "m1") == 1
@test metadata(mda, :m1) == 1
@test hasproperty(mda, "annotation")
@test hasproperty(mda, :annotation)
@test metadatakeys(mda) == propertynames(mda) == keys(md)
@test all(mda .== a)
@test all(mda .== mda)
@test metadata(mda[:,1], :annotation) == md.annotation

@test metadata(@inferred(Base.adjoint(mda)), :annotation) == md.annotation
@test metadata(@inferred(Base.adjoint(mda[:, 1])), :annotation) == md.annotation
@test metadata(@inferred(permutedims(mda, (2, 1))), :annotation) == md.annotation

