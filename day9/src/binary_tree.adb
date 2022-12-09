pragma SPARK_Mode (On);

package body Binary_Tree is
   procedure Add (Item : Element_Type; Node : in out Tree_Node_Ptr)
   is
   begin
      if Node = null then
         Node := new Tree_Node;
         Node.Data := Item;
         return;
      end if;

      if Item = Node.Data then
         return;
      elsif Item < Node.Data then
         if Node.Left = null then
            Node.Left := new Tree_Node;
            Node.Left.Data := Item;
            return;
         else
            Add (Item, Node.Left);
         end if;
      else
         if Node.Right = null then
            Node.Right := new Tree_Node;
            Node.Right.Data := Item;
            return;
         else
            Add (Item, Node.Right);
         end if;
      end if;
   end Add;

   function Contains (Item : Element_Type; Node : Tree_Node_Ptr)
      return Boolean
   is
   begin
      if Node = null then
         return False;
      else
         if Node.Data = Item then
            return True;
         elsif Item < Node.Data then
            return Contains (Item, Node.Left);
         else
            return Contains (Item, Node.Right);
         end if;
      end if;
   end Contains;

   procedure Lookup (Item : Element_Type; Node : Tree_Node_Ptr;
         Found : out Boolean; Found_Item : out Element_Type)
   is
   begin
      if Node = null then
         Found := False;
         Found_Item := Default_Element;
      else
         if Node.Data = Item then
            Found := True;
            Found_Item := Node.Data;
         elsif Item < Node.Data then
            Lookup (Item, Node.Left, Found, Found_Item);
         else
            Lookup (Item, Node.Right, Found, Found_Item);
         end if;
      end if;
   end Lookup;

   procedure Free_Tree (Node : in out Tree_Node_Ptr)
   is
   begin
      if Node.Left /= null then
         Free_Tree (Node.Left);
         Free (Node.Left);
      end if;

      if Node.Right /= null then
         Free_Tree (Node.Right);
         Free (Node.Right);
      end if;

      Free (Node);
   end Free_Tree;
end Binary_Tree;
