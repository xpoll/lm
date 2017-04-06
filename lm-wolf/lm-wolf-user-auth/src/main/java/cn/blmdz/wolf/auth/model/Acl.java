package cn.blmdz.wolf.auth.model;

import java.io.Serializable;
import java.util.Map;

import lombok.Data;

@Data
public class Acl implements Serializable {
   private static final long serialVersionUID = 0L;
   private Map<String, DefaultRole> defaults;
   private Map<String, Resource> resources;
   private Map trees;
}
