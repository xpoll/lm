package cn.blmdz.wolf.auth.model;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.wolf.auth.model.Resource;
import lombok.Data;

@Data
public class RequestResource implements Resource, Serializable {
   private static final long serialVersionUID = 1L;
   private String name;
   private String description;
   private List<Request> requests;
}
