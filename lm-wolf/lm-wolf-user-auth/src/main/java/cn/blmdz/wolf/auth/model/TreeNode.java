package cn.blmdz.wolf.auth.model;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import lombok.Data;

@Data
public class TreeNode implements Serializable {
   private static final long serialVersionUID = 1L;
   private String name;
   private String description;
   private List<String> resources;
   private Map<String, TreeNode> children;
}
