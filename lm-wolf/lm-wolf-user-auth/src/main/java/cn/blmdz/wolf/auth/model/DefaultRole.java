package cn.blmdz.wolf.auth.model;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

@Data
public class DefaultRole implements Serializable {
   private static final long serialVersionUID = 1L;
   private String name;
   private String description;
   private List<String> resources;
   private List<Request> requests;
}
