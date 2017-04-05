package io.terminus.parana.order.dto;

import io.terminus.parana.order.model.Order;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class OrderChain implements Serializable {
   private static final long serialVersionUID = -810926732593896472L;
   private Order order;
   private Map context;
   private List children = null;

   public boolean hasChildren() {
      return null != this.children && !this.children.isEmpty();
   }

   public Order getOrder() {
      return this.order;
   }

   public Map getContext() {
      return this.context;
   }

   public List getChildren() {
      return this.children;
   }

   public void setOrder(Order order) {
      this.order = order;
   }

   public void setContext(Map context) {
      this.context = context;
   }

   public void setChildren(List children) {
      this.children = children;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof OrderChain)) {
         return false;
      } else {
         OrderChain other = (OrderChain)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$order = this.getOrder();
            Object other$order = other.getOrder();
            if(this$order == null) {
               if(other$order != null) {
                  return false;
               }
            } else if(!this$order.equals(other$order)) {
               return false;
            }

            Object this$context = this.getContext();
            Object other$context = other.getContext();
            if(this$context == null) {
               if(other$context != null) {
                  return false;
               }
            } else if(!this$context.equals(other$context)) {
               return false;
            }

            Object this$children = this.getChildren();
            Object other$children = other.getChildren();
            if(this$children == null) {
               if(other$children != null) {
                  return false;
               }
            } else if(!this$children.equals(other$children)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof OrderChain;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $order = this.getOrder();
      result = result * 59 + ($order == null?0:$order.hashCode());
      Object $context = this.getContext();
      result = result * 59 + ($context == null?0:$context.hashCode());
      Object $children = this.getChildren();
      result = result * 59 + ($children == null?0:$children.hashCode());
      return result;
   }

   public String toString() {
      return "OrderChain(order=" + this.getOrder() + ", context=" + this.getContext() + ", children=" + this.getChildren() + ")";
   }
}
