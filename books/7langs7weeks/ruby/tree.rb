class Tree
    attr_accessor :children, :node_name

    def initialize(hash)
        if hash.keys.length > 1
            raise ArgumentError.new("Tree has more than one root. Found %s" % hash.keys.to_s)
        end
        @node_name = hash.keys.first
        @children = hash.values.first.collect do |name, childrenhash|
            Tree.new({name => childrenhash})
        end
    end

    def visit_all(&block)
        visit &block
        children.each {|c| c.visit_all &block}
    end

    def visit(&block)
        block.call self
    end
end

# Correct output:
#
# grandpa
# dad
# child 1
# child 2
# uncle
# child 3
# child 4

hash = {'grandpa' => { 'dad' => {'child 1' => {}, 'child 2' => {} }, 'uncle' => {'child 3' => {}, 'child 4' => {} } } }
t = Tree.new(hash)
t.visit_all {|node| puts node.node_name}
