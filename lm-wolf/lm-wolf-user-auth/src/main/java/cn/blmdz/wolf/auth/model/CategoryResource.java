package cn.blmdz.wolf.auth.model;

import java.io.Serializable;
import java.util.HashMap;

import cn.blmdz.wolf.auth.model.Resource;

public class CategoryResource extends HashMap<Object, RequestResource> implements Resource, Serializable {
   private static final long serialVersionUID = 0L;
}
