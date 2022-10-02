using MetadataArrays, Test

@testset "MetadataVector" begin
    v = [1, 3, 5, 4]
    mdv = MetadataVector(v, "Numbers")
    @test length(mdv) == 4
    @test collect(mdv) == parent(mdv) == copy(mdv)
    @test metadata(mdv) == "Numbers"
    # s_view1 = view(mdv, 1:3)
    # s_view2 = view(s_view1, 1:2)
    # @test metadata(s_view1) == metadata(s_view2) == "Numbers"
    mdv[3] = 50
    @test all(mdv .== [1, 3, 50, 4])
    @test eltype(mdv) == Int
    # @test metadata(similar(mdv)) == metadata(mdv)
end

@testset "MetadataArray" begin
    a = [1 2; 3 4; 5 4]
    mda = MetadataArray(a, Dict(1 => "Not at all", 10 => "A lot"))
    @test size(mda) == size(a)
    @test mda[6] == parent(mda)[6]
    @test collect(mda) == parent(mda) == v == copy(mda)
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

@testset "properties" begin
    a = ones(4, 4);
    md1 = (m1 = 1, m2 = [1, 2]);
    mda1 = MetadataArray(a, md1)
    @test mda1.m1 == md1.m1
    @test mda1."m1" == md1.m1
    @test propertynames(mda1) == propertynames(md1)
    @test hasproperty(mda1, :m1) == hasproperty(md1, :m1)
    @test hasproperty(mda1, "m1") == hasproperty(md1, :m1)

    md2 = Dict("m1" => 1, "m2" => [1, 2]);
    mda2 = MetadataArray(a, md2)

    @test mda2.m1 == md2["m1"]
    @test mda2."m1" == md2["m1"]
    @test propertynames(mda2) == keys(md2)
    @test hasproperty(mda2, :m1) == haskey(md2, "m1")
    @test hasproperty(mda2, "m1") == haskey(md2, "m1")
end
