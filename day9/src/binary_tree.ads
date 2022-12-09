pragma SPARK_Mode (On);

with Ada.Unchecked_Deallocation;

generic
   type Element_Type is private;
   Default_Element : Element_Type;
   with function "<" (X, Y : Element_Type) return Boolean;
   with function "=" (X, Y : Element_Type) return Boolean;

package Binary_Tree is

   type Tree_Node;
   type Tree_Node_Ptr is access Tree_Node;

   type Tree_Node is record
      Data : Element_Type := Default_Element;
      Left : Tree_Node_Ptr := null;
      Right : Tree_Node_Ptr := null;
   end record;

   function Contains (Item : Element_Type; Node : Tree_Node_Ptr)
      return Boolean;

   procedure Lookup (Item : Element_Type; Node : Tree_Node_Ptr;
      Found : out Boolean; Found_Item : out Element_Type);

   procedure Free is new Ada.Unchecked_Deallocation (Tree_Node, Tree_Node_Ptr);

   procedure Add (Item : Element_Type; Node : in out Tree_Node_Ptr)
      with
      Post => Node /= null;

   procedure Free_Tree (Node : in out Tree_Node_Ptr)
      with
      Pre => Node /= null,
      Post => Node = null;
end Binary_Tree;
